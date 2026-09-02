(define-module (guile-dmenu vui-adapter)
  #:use-module (guile-dmenu filter)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (vui element)
  #:export (completion-state->vui-model
            completion-model->vui-tree
            completion-state->vui-tree
            vui-action->completion-event
            dispatch-completion-vui-action))

(define (copy-tree value)
  (cond ((string? value) (string-copy value))
        ((pair? value) (cons (copy-tree (car value))
                             (copy-tree (cdr value))))
        ((vector? value) (list->vector (map copy-tree (vector->list value))))
        (else value)))

(define (completion-state->vui-model state)
  "Return a detached, immutable-data snapshot of completion STATE."
  (unless (completing-read-state? state)
    (error "expected completing-read state" state))
  (map (lambda (entry) (cons (car entry) (copy-tree (cdr entry))))
       `((input-text . ,(completing-read-state-input-text state))
         (cursor-position . ,(completing-read-state-cursor-position state))
         (selected-index . ,(completing-read-state-selected-index state))
         (filtered-options . ,(completing-read-state-filtered-options state))
         (confirmation-input . ,(completing-read-state-confirmation-input state))
         (completion-invoked? . ,(completing-read-state-completion-invoked? state))
         (defaults . ,(completing-read-state-defaults state))
         (default-index . ,(completing-read-state-default-index state))
         (inherit-input-method? . ,(completing-read-state-inherit-input-method? state))
         (history-position . ,(completing-read-state-history-position state))
         (completion-metadata . ,(completing-read-state-completion-metadata state))
         (completion-boundaries . ,(completing-read-state-completion-boundaries state)))))

(define (completion-model->vui-tree model)
  "Express a completion snapshot as a deterministic, presentation-neutral tree."
  (let ((selected (assq-ref model 'selected-index)))
    (column
     (cons (text-input (assq-ref model 'input-text)
                       #:key 'completion-input #:on-change 'replace-input)
           (map (lambda (option index)
                  (button option #:key index #:action 'select
                          #:disabled? (not (= index selected))))
                (assq-ref model 'filtered-options)
                (iota (length (assq-ref model 'filtered-options)))))
     #:key 'completion)))

(define (completion-state->vui-tree state)
  (completion-model->vui-tree (completion-state->vui-model state)))

(define (vui-action->completion-event action)
  "Translate a completion VUI action to the established domain event vocabulary."
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
    ((and (or 'left 'right 'home 'end 'backspace 'ctrl+backspace
              'next-default 'previous-default) event)
     event)
    (_ #f)))

(define* (dispatch-completion-vui-action state action options collection
                                         #:key (predicate #f)
                                         (selection-mode 'text)
                                         (require-match #f)
                                         (style 'substring)
                                         (input-enabled? #t))
  "Translate ACTION and delegate all completion semantics to the domain layer."
  (let ((event (vui-action->completion-event action)))
    (cond ((eq? event 'cancel) (list 'cancelled))
          ((not event) (list 'no-change state))
          (else
           (dispatch-completing-read-event
            state event options collection
            #:predicate predicate #:selection-mode selection-mode
            #:require-match require-match #:style style
            #:input-enabled? input-enabled?)))))
