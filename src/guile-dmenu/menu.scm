(define-module (guile-dmenu menu)
  #:use-module (guile-dmenu startup)
  #:use-module (guile-dmenu graphics)
  #:use-module (guile-dmenu filter)
  #:use-module (guile-dmenu completion)
  #:use-module (guile-dmenu vui-adapter)
  #:use-module (vui guile-wayland)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (completing-read
            completing-read/result
            completing-read-result?
            completing-read-result-status
            completing-read-result-value
            completing-read-result-status-procedure
            completing-read-result-value-procedure
            menu-width menu-padding menu-max-options)
  #:re-export (make-completion-history
               completion-history?
               completion-history-entries
               set-completion-history-entries!
               completion-history-length
               completion-history-add!))

(define menu-width (make-parameter 800))
(define menu-padding (make-parameter 4))
;; Keep the default surface bounded even for very large candidate sets.
;; Callers and the dmenu -l option can still choose a different limit.
(define menu-max-options (make-parameter 10))

(define-record-type <completing-read-result>
  (make-completing-read-result status value)
  completing-read-result?
  (status completing-read-result-status)
  (value completing-read-result-value))

(define (answered value) (make-completing-read-result 'answered value))
(define (terminated status) (make-completing-read-result status #f))

;; SRFI-9 accessors are syntax transformers in Guile.  These procedural
;; bridges support callers that resolve the graphical module lazily.
(define (completing-read-result-status-procedure result)
  (completing-read-result-status result))
(define (completing-read-result-value-procedure result)
  (completing-read-result-value result))

(define (model-ref model key)
  (assq-ref model key))

(define (model-set model key value)
  (acons key value (alist-delete key model)))

(define (color-component->hex component)
  (let* ((byte (inexact->exact (round (* 255 (max 0 (min 1 component))))))
         (hex (string-upcase (number->string byte 16))))
    (if (= (string-length hex) 1) (string-append "0" hex) hex)))

(define (color->vui color)
  "Convert guile-dmenu's normalized RGB triples to Guile-VUI hex colors."
  (unless (and (list? color) (= (length color) 3) (every real? color))
    (error "expected an RGB color triple" color))
  (string-append "#" (string-concatenate (map color-component->hex color))))

(define* (completing-read/result prompt collection
                          #:optional (predicate #f) (require-match #f)
                          (initial-input "") (history #f) (default #f)
                          (inherit-input-method #f)
                          #:key (message #f) (input-enabled? #t)
                          (option-details #f)
                          (comment-on-tab? #f)
                          (escape-action 'cancel)
                          (timeout #f) (max-message-lines 12)
                          (selection-mode 'text)
                          (initial-selected-index 0)
                          (completion-style 'substring))
  "Run the production completing reader on guile-vui's Wayland host."
  (unless (memq selection-mode '(text menu menu-index))
    (error "unsupported selection mode" selection-mode))
  (unless (boolean? comment-on-tab?)
    (error "comment-on-tab marker must be boolean" comment-on-tab?))
  (unless (memq escape-action '(cancel back))
    (error "escape action must be cancel or back" escape-action))
  (lookup-completion-style completion-style)
  (let ((normalized-initial
         (call-with-values
             (lambda () (normalize-initial-input initial-input)) list)))
    (normalize-defaults default)
    (call-with-values (lambda () (normalize-history history)) list)
    (let* ((candidates (normalize-collection collection predicate
                                            (car normalized-initial)))
           (options (map completion-candidate-display candidates))
           (maximum (or (menu-max-options) (length options)))
           (message-lines
            (lambda ()
              (if message
                  (wrap-message-to-width message (menu-width)
                                         (menu-padding) max-message-lines)
                  '())))
           (started (get-internal-real-time))
           (deadline (and timeout
                          (+ started (* timeout internal-time-units-per-second)))))
      (unless (or (not option-details)
                  (and (list? option-details)
                       (= (length option-details) (length options))
                       (every (lambda (detail)
                                (or (not detail) (string? detail)))
                              option-details)))
        (error "option details must match the displayed collection"
               option-details))
      (report-startup-phase! 'collection-normalized)
      (let ((initial-model
             `((state . ,(initial-state options initial-input default
                                       inherit-input-method history
                                       initial-selected-index))
               (comment-state . #f)
               (comment-index . #f)
               (details-visible? . #f)
               (result . #f))))
        (report-startup-phase! 'initial-state-constructed)
        (define (finish model result)
          (model-set model 'result result))
        (define (update-domain model state-key action domain-options
                               domain-collection domain-mode domain-input?)
          (let ((outcome
                 (dispatch-completion-vui-action
                  (model-ref model state-key) action
                  domain-options domain-collection
                  #:predicate predicate #:selection-mode domain-mode
                  #:require-match require-match #:style completion-style
                  #:input-enabled? domain-input?)))
            (match outcome
              (('selected value) (finish model (answered value)))
              (('state-update next) (model-set model state-key next))
              (_ model))))
        (define (update model action)
          (let ((state (model-ref model 'state))
                (comment-state (model-ref model 'comment-state)))
            (match action
              ('cancel
               (if comment-state
                   (model-set
                    (model-set model 'comment-state #f) 'comment-index #f)
                   (finish model
                           (if (eq? escape-action 'back)
                               (answered 'back)
                               (terminated 'cancelled)))))
              ('select
               (report-runtime-event! 'select-received)
               (if comment-state
                   (let ((text (completing-read-state-input-text comment-state)))
                     (if (string-null? text) model
                         (finish model
                                 (answered
                                  (list 'comment
                                        (model-ref model 'comment-index)
                                        text)))))
                   (update-domain model 'state action options collection
                                  selection-mode input-enabled?)))
              ('complete
               (cond
                (comment-state model)
                ((and (not input-enabled?) comment-on-tab?)
                 (model-set
                  (model-set model 'comment-index
                             (completing-read-state-selected-index state))
                  'comment-state
                  (initial-state '() "" #f #f #f 0)))
                ((and (not input-enabled?) option-details)
                 (model-set model 'details-visible?
                            (not (model-ref model 'details-visible?))))
                (else
                 (update-domain model 'state action options collection
                                selection-mode input-enabled?))))
              ('host-timeout (finish model (terminated 'timed-out)))
              ('host-close (finish model (terminated 'window-closed)))
              (('host-failure . _)
               (finish model (terminated 'graphical-failure)))
              (_
               (if comment-state
                   (update-domain model 'comment-state action '() '()
                                  'text #t)
                   (update-domain model 'state action options collection
                                  selection-mode input-enabled?))))))
        (define (view model)
          (let* ((input-state (or (model-ref model 'comment-state)
                                  (model-ref model 'state)))
                 (input-lines
                  (if (input-line-wrapping?)
                      (wrapped-text-line-count
                       (completing-read-state-input-text input-state)
                       (- (menu-width) (* 2 (menu-padding))
                          (* 2 (border-width))))
                      1)))
            (completion-state->vui-tree
             (model-ref model 'state)
           #:prompt prompt #:maximum maximum #:message-lines (message-lines)
           #:option-details option-details
           #:details-visible? (model-ref model 'details-visible?)
           #:input-enabled? input-enabled?
           #:comment-state (model-ref model 'comment-state)
           #:comment-index (model-ref model 'comment-index)
           #:width (menu-width) #:padding (menu-padding)
           #:row-height (item-height (menu-padding))
           #:fixed-height? (fixed-height?)
           #:normal-background (color->vui (background-color))
           #:normal-foreground (color->vui (foreground-color))
           #:highlight-background (color->vui (highlight-background-color))
           #:highlight-foreground (color->vui (highlight-foreground-color))
           #:filter-background (and (filter-background-color)
                                    (color->vui (filter-background-color)))
           #:filter-foreground (and (filter-foreground-color)
                                    (color->vui (filter-foreground-color)))
           #:cursor-color (and (cursor-color) (color->vui (cursor-color)))
           #:title-background (and (title-background-color)
                                   (color->vui (title-background-color)))
           #:title-foreground (and (title-foreground-color)
                                   (color->vui (title-foreground-color)))
           #:alternate-background (and (alt-background-color)
                                       (color->vui (alt-background-color)))
           #:alternate-foreground (and (alt-foreground-color)
                                       (color->vui (alt-foreground-color)))
           #:border-color (color->vui (border-color))
           #:border-width (border-width) #:selected-prefix (prefix-text)
             #:input-wrapping? (input-line-wrapping?)
             #:input-line-count input-lines)))
        (define (logical-size model)
          (let* ((snapshot
                  (completion-state->vui-model
                   (model-ref model 'state) #:maximum maximum
                   #:message-lines (message-lines) #:option-details option-details
                   #:details-visible? (model-ref model 'details-visible?)
                   #:input-enabled? input-enabled?
                   #:comment-state (model-ref model 'comment-state)
                   #:comment-index (model-ref model 'comment-index)
                   #:fixed-height? (fixed-height?)))
                 (input-state (or (model-ref model 'comment-state)
                                  (model-ref model 'state)))
                 (input-lines
                  (if (input-line-wrapping?)
                      (wrapped-text-line-count
                       (completing-read-state-input-text input-state)
                       (- (menu-width) (* 2 (menu-padding))
                          (* 2 (border-width))))
                      1))
                 (rows (+ (if (input-line-wrapping?) (+ 1 input-lines) 1)
                          (assq-ref snapshot 'reserved-option-rows)
                          (length (assq-ref snapshot 'message-lines)))))
            (list (menu-width) (* rows (item-height (menu-padding))))))
        (define (lifecycle event . arguments)
          (case event
            ((start) (report-startup-phase! 'window-created))
            ((configure)
             (let* ((payload (and (pair? arguments) (car arguments)))
                    (width (and (list? payload) (assq-ref payload 'width))))
               (when (and width (positive? width)) (menu-width width)))
             (report-runtime-event! 'surface-configured)
             (report-startup-phase! 'first-frame-committed))
            ((stop) (report-runtime-event! 'result-received))
            (else #t)))
        (catch #t
          (lambda ()
            (run-wayland-vui
             initial-model update view #:title "dmenu" #:app-id "wl-dmenu"
             #:logical-size logical-size #:route-key route-key #:pointer? #f
             #:font "monospace 14" #:timeout-action 'host-timeout
             #:close-action 'host-close
             #:failure-action
             (lambda (error) (cons 'host-failure error))
             #:timeout? (lambda ()
                          (and deadline
                               (>= (get-internal-real-time) deadline)))
             #:cursor-indices
             (lambda (model _focus)
               (let ((state (or (model-ref model 'comment-state)
                                (model-ref model 'state))))
                 `((completion-input
                    . ,(completing-read-state-cursor-position state)))))
             #:terminal? (lambda (model) (and (model-ref model 'result) #t))
             #:result (lambda (model) (model-ref model 'result))
             #:lifecycle lifecycle))
          (lambda args
            (report-runtime-event! 'graphical-startup-failure)
            (terminated 'graphical-failure)))))))

(define (completing-read . arguments)
  "Compatibility wrapper returning the answer, or #f on termination."
  (let ((result (apply completing-read/result arguments)))
    (and (eq? (completing-read-result-status result) 'answered)
         (completing-read-result-value result))))
