(define-module (guile-dmenu swing-adapter)
  #:use-module (guile-dmenu filter)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (swing element)
  #:use-module (swing style)
  #:export (completion-state->swing-model
            completion-model->swing-tree
            completion-state->swing-tree
            route-key
            swing-action->completion-event
            dispatch-completion-swing-action))

(define (route-key symbol text modifiers)
  "Translate one normalized Swing key press to guile-dmenu's action vocabulary.
Return #f for modified/non-text keys that guile-dmenu intentionally consumes.
The function is pure so the Swing input controller can use it identically for
the initial press and every repeat without acquiring completion state."
  (unless (and (symbol? symbol) (string? text) (list? modifiers)
               (every symbol? modifiers))
    (error "invalid normalized Swing key" symbol text modifiers))
  (let ((control? (and (memq 'control modifiers) #t))
        (alt? (and (memq 'alt modifiers) #t))
        (shift? (and (memq 'shift modifiers) #t)))
    (cond
     ((or (eq? symbol 'Escape)
          (and control? (memq symbol '(c g))))
      'cancel)
     ((memq symbol '(Return KP_Enter)) 'select)
     ((or (eq? symbol 'ISO_Left_Tab)
          (and (eq? symbol 'Tab) shift?))
      'complete-previous)
     ((eq? symbol 'Tab) 'complete)
     ((eq? symbol 'Left) 'left)
     ((eq? symbol 'Right) 'right)
     ((eq? symbol 'Home) 'home)
     ((eq? symbol 'End) 'end)
     ((and alt? (eq? symbol 'n)) 'next-default)
     ((and alt? (eq? symbol 'p)) 'previous-default)
     ((or (eq? symbol 'Down) (and control? (eq? symbol 'n))) 'next)
     ((or (eq? symbol 'Up) (and control? (eq? symbol 'p))) 'previous)
     ((eq? symbol 'BackSpace)
      (if control? 'ctrl+backspace 'backspace))
     ((or control? alt?) #f)
     ((not (string-null? text))
      (list 'input-text text))
     (else #f))))

(define (copy-tree value)
  (cond ((string? value) (string-copy value))
        ((pair? value) (cons (copy-tree (car value))
                             (copy-tree (cdr value))))
        ((vector? value) (list->vector (map copy-tree (vector->list value))))
        (else value)))

(define (valid-string-list? value)
  (and (list? value) (every string? value)))

(define (visible-window options selected maximum)
  (let* ((count (length options))
         (page-start (if (and (positive? maximum) (positive? count))
                         (* (quotient selected maximum) maximum)
                         0))
         ;; Match the legacy renderer: the last page is filled when possible.
         (start (min page-start (max 0 (- count maximum))))
         (length (min maximum (- count start))))
    (values start (take (drop options start) length))))

(define* (completion-state->swing-model state
                                      #:key (prompt "") (maximum 10)
                                      (message-lines '()) (option-details #f)
                                      (details-visible? #f) (input-enabled? #t)
                                      (comment-state #f) (comment-index #f)
                                      (width 800) (padding 4)
                                      (row-height (+ 19 (* 2 padding)))
                                      (fixed-height? #f)
                                      (normal-background 'black)
                                      (normal-foreground 'white)
                                      (highlight-background 'blue)
                                      (highlight-foreground 'white)
                                      (filter-background #f)
                                      (filter-foreground #f)
                                      (cursor-color #f)
                                      (title-background #f)
                                      (title-foreground #f)
                                      (alternate-background #f)
                                      (alternate-foreground #f)
                                      (border-color 'black) (border-width 0)
                                      (selected-prefix "")
                                      (input-wrapping? #f)
                                      (input-line-count 1))
  "Return a detached, immutable-data snapshot of completion STATE."
  (unless (completing-read-state? state)
    (error "expected completing-read state" state))
  (unless (and (string? prompt) (exact-integer? maximum) (>= maximum 0)
               (valid-string-list? message-lines)
               (or (not option-details)
                   (and (list? option-details)
                        (every (lambda (x) (or (not x) (string? x)))
                               option-details)))
               (boolean? details-visible?) (boolean? input-enabled?)
               (or (not comment-state) (completing-read-state? comment-state))
               (or (not comment-index)
                   (and (exact-integer? comment-index) (>= comment-index 0)))
               (real? width) (>= width 0) (real? padding) (>= padding 0)
               (real? row-height) (>= row-height 0)
               (boolean? fixed-height?)
               (every (lambda (color) (or (not color) (string? color) (symbol? color)))
                      (list normal-background normal-foreground
                            highlight-background highlight-foreground
                            filter-background filter-foreground cursor-color
                            title-background title-foreground
                            alternate-background alternate-foreground
                            border-color))
               (real? border-width) (>= border-width 0)
               (string? selected-prefix) (boolean? input-wrapping?)
               (exact-integer? input-line-count) (positive? input-line-count))
    (error "invalid completion presentation" state))
  (let* ((options (completing-read-state-filtered-options state))
         (selected (completing-read-state-selected-index state)))
    (call-with-values
        (lambda () (visible-window options selected maximum))
      (lambda (start visible)
        (let* ((commenting? (and comment-state #t))
               (detail (and (or details-visible? (not input-enabled?))
                            (pair? option-details)
                            (< selected (length option-details))
                            (list-ref option-details selected)))
               (messages
                (append (if commenting?
                            (cons "Comment on selected choice  ·  Enter attach  ·  Esc back"
                                  (if (pair? message-lines)
                                      (cdr message-lines) '()))
                            message-lines)
                        (if detail (list detail) '())
                        (if (completing-read-state-confirmation-input state)
                            '("Confirm submission with RET") '()))))
          (map (lambda (entry) (cons (car entry) (copy-tree (cdr entry))))
       `((prompt . ,(if commenting? "Comment ›" prompt))
         (mode . ,(cond (commenting? 'comment)
                        (detail 'details)
                        (else 'menu)))
         (input-text . ,(completing-read-state-input-text
                         (or comment-state state)))
         (input-enabled? . ,(or commenting? input-enabled?))
         (cursor-position . ,(completing-read-state-cursor-position
                              (or comment-state state)))
         (selected-index . ,selected)
         (visible-start . ,start)
         (visible-selected-index . ,(- selected start))
         (visible-options . ,visible)
         (message-lines . ,messages)
         (maximum . ,maximum)
         (reserved-option-rows . ,(if fixed-height? maximum (length visible)))
         (row-height . ,row-height)
         (normal-background . ,normal-background)
         (normal-foreground . ,normal-foreground)
         (highlight-background . ,highlight-background)
         (highlight-foreground . ,highlight-foreground)
         (filter-background . ,filter-background)
         (filter-foreground . ,filter-foreground)
         (cursor-color . ,cursor-color)
         (title-background . ,title-background)
         (title-foreground . ,title-foreground)
         (alternate-background . ,alternate-background)
         (alternate-foreground . ,alternate-foreground)
         (border-color . ,border-color)
         (border-width . ,border-width)
         (selected-prefix . ,selected-prefix)
         (input-wrapping? . ,input-wrapping?)
         (input-line-count . ,input-line-count)
         (width . ,width)
         (padding . ,padding)
         (comment-index . ,comment-index)
         (filtered-options . ,options)
         (confirmation-input . ,(completing-read-state-confirmation-input state))
         (completion-invoked? . ,(completing-read-state-completion-invoked? state))
         (defaults . ,(completing-read-state-defaults state))
         (default-index . ,(completing-read-state-default-index state))
         (inherit-input-method? . ,(completing-read-state-inherit-input-method? state))
         (history-position . ,(completing-read-state-history-position state))
         (completion-metadata . ,(completing-read-state-completion-metadata state))
         (completion-boundaries . ,(completing-read-state-completion-boundaries state)))))))))

(define (completion-model->swing-tree model)
  "Express a completion snapshot as a deterministic, presentation-neutral tree."
  (let* ((selected (assq-ref model 'selected-index))
         (start (assq-ref model 'visible-start))
         (padding (assq-ref model 'padding))
         (root-style (make-style `((width . ,(assq-ref model 'width))
                                   (padding . ,padding)
                                   (border-width . ,(assq-ref model 'border-width))
                                   (border-color . ,(assq-ref model 'border-color))
                                   (background-color
                                    . ,(assq-ref model 'normal-background)))))
         (text-style
          (make-style `((color . ,(assq-ref model 'normal-foreground)))))
         (prompt-style
          (make-style `((background-color . ,(or (assq-ref model 'title-background)
                                                 (assq-ref model 'normal-background)))
                        (color . ,(or (assq-ref model 'title-foreground)
                                     (assq-ref model 'normal-foreground))))))
         (input-style
          (make-style `((flex-grow . 1)
                        (height . ,(* (assq-ref model 'input-line-count)
                                      (assq-ref model 'row-height)))
                        (background-color . ,(or (assq-ref model 'filter-background)
                                                (assq-ref model 'normal-background)))
                        (color . ,(or (assq-ref model 'filter-foreground)
                                     (assq-ref model 'normal-foreground)))
                        (cursor-color . ,(or (assq-ref model 'cursor-color)
                                            (assq-ref model 'normal-foreground))))))
         (option-style
          (lambda (selected? absolute)
            (make-style
             `((height . ,(assq-ref model 'row-height))
               (background-color . ,(if selected?
                                        (assq-ref model 'highlight-background)
                                        (or (and (odd? absolute)
                                                 (assq-ref model 'alternate-background))
                                            (assq-ref model 'normal-background))))
               (color . ,(if selected?
                             (assq-ref model 'highlight-foreground)
                             (or (and (odd? absolute)
                                      (assq-ref model 'alternate-foreground))
                                 (assq-ref model 'normal-foreground))))))))
         (visible (assq-ref model 'visible-options)))
    (column
     (append
      (let ((prompt (text (assq-ref model 'prompt) #:key 'prompt
                          #:style prompt-style))
            (input (text-input (assq-ref model 'input-text)
                               #:key 'completion-input
                               #:style input-style
                               #:on-change 'replace-input
                               #:disabled?
                               (not (assq-ref model 'input-enabled?)))))
        (if (assq-ref model 'input-wrapping?)
            (list prompt input)
            (list (row (list prompt input) #:key 'input-row
                       #:style (make-style
                                `((height . ,(assq-ref model 'row-height))))))))
      (map (lambda (line index)
             (text line #:key (string-append "message-" (number->string index))
                   #:style text-style))
           (assq-ref model 'message-lines)
           (iota (length (assq-ref model 'message-lines))))
      (list
       (list-element
        (append
         (map (lambda (option offset)
                (let ((absolute (+ start offset)))
                  (button (if (= absolute selected)
                              (string-append (assq-ref model 'selected-prefix) option)
                              option)
                          #:key absolute #:action 'select
                          #:style (option-style (= absolute selected) absolute)
                          #:disabled? (not (= absolute selected)))))
              visible (iota (length visible)))
         (map (lambda (offset)
                (spacer #:key (string-append "reserved-row-"
                                             (number->string offset))
                        #:height (assq-ref model 'row-height)))
              (iota (- (assq-ref model 'reserved-option-rows)
                       (length visible)))))
        #:key 'candidates)))
     #:key 'completion #:style root-style)))

(define* (completion-state->swing-tree state #:rest presentation)
  (completion-model->swing-tree
   (apply completion-state->swing-model state presentation)))

(define (swing-action->completion-event action)
  "Translate a completion Swing action to the established domain event vocabulary."
  (match action
    ((or 'select 'accept) 'select)
    ((or 'next 'move-next) 'next)
    ((or 'previous 'move-previous) 'previous)
    ((or 'complete 'complete-next) 'complete)
    ('complete-previous 'complete-previous)
    ((or 'cancel 'escape) 'cancel)
    (('replace-input (? string? value) (? exact-integer? cursor))
     (list 'replace-input value cursor))
    (('input-char (? char? character)) (list 'input-char character))
    (('input-text (? string? text))
     (and (not (string-null? text)) (list 'input-text text)))
    ((and (or 'left 'right 'home 'end 'backspace 'ctrl+backspace
              'next-default 'previous-default) event)
     event)
    (_ #f)))

(define* (dispatch-completion-swing-action state action options collection
                                         #:key (predicate #f)
                                         (selection-mode 'text)
                                         (require-match #f)
                                         (style 'substring)
                                         (input-enabled? #t))
  "Translate ACTION and delegate all completion semantics to the domain layer."
  (let ((event (swing-action->completion-event action)))
    (cond ((eq? event 'cancel) (list 'cancelled))
          ((not event) (list 'no-change state))
          ((and (pair? event) (eq? (car event) 'input-text))
           ;; Swing's normalized text may contain more than one character (for
           ;; example from compose).  Replay it through the domain's existing
           ;; character transition so filtering and cursor ownership stay in
           ;; guile-dmenu and no normalized text is discarded.
           (fold (lambda (character result)
                   (if (eq? (car result) 'state-update)
                       (dispatch-completing-read-event
                        (cadr result) (list 'input-char character)
                        options collection #:predicate predicate
                        #:selection-mode selection-mode
                        #:require-match require-match #:style style
                        #:input-enabled? input-enabled?)
                       result))
                 (list 'state-update state)
                 (string->list (cadr event))))
          (else
           (dispatch-completing-read-event
            state event options collection
            #:predicate predicate #:selection-mode selection-mode
            #:require-match require-match #:style style
            #:input-enabled? input-enabled?)))))
