(define-module (guile-dmenu structured-prompt)
  #:use-module (guile-dmenu question)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (ask-questions
            ask-questions/result
            confirm
            confirm/result
            question-result?
            question-result-status
            question-result-answers))

(define %back-label "← Back")
(define %other-label "Other…")

(define-record-type <question-result>
  (make-question-result status answers)
  question-result?
  (status question-result-status)
  (answers question-result-answers))

(define (monotonic-seconds)
  (/ (get-internal-real-time) internal-time-units-per-second))

(define (graphical-reader . arguments)
  (catch #t
    (lambda ()
      (let* ((menu (resolve-interface '(guile-dmenu menu)))
             (read (module-ref menu 'completing-read/result))
             (status
              (module-ref menu 'completing-read-result-status-procedure))
             (value
              (module-ref menu 'completing-read-result-value-procedure))
             (result (apply read arguments)))
        (list 'reader-result (status result) (value result))))
    (lambda args '(reader-result graphical-failure #f))))

(define (reader-result answer)
  ;; Injected legacy readers may still return a value or #f.  The graphical
  ;; reader exposes the richer result without coupling this module at load time
  ;; to Wayland.
  (cond ((and (list? answer) (= (length answer) 3)
              (eq? (car answer) 'reader-result))
         (unless (memq (cadr answer)
                       '(answered cancelled timed-out window-closed
                                  graphical-failure))
           (error "reader returned an invalid termination status"
                  (cadr answer)))
         (values (cadr answer) (caddr answer)))
          ((not answer) (values 'cancelled #f))
          (else (values 'answered answer))))

(define (option-display option)
  (string-append
   (question-option-label option)
   (if (question-option-recommended? option) " (Recommended)" "")
   (if (question-option-description option)
       (string-append " — " (question-option-description option))
       "")))

(define (question-message question page total context)
  (string-join
   (append
    (list (format #f "Question ~a of ~a  ·  ↑↓ choose  ·  Tab add comment  ·  Enter confirm  ·  Esc cancel"
                  (+ page 1) total))
    (if context (list context) '()))
   "\n"))

(define (option-index options selected)
  (and (or (question-option? selected)
           (question-comment-answer? selected))
       (list-index
        (lambda (option)
          (equal? (question-option-id option)
                  (question-option-id
                   (if (question-comment-answer? selected)
                       (question-comment-answer-option selected)
                       selected))))
        options)))

(define (initial-choice-index state question options)
  (or (option-index options (question-state-selected-option state))
      (and (question-other-answer? (question-state-selected-option state))
           (single-question-allow-other? question)
           (length options))
      (list-index question-option-recommended? options)
      0))

(define (advance-or-complete state page total)
  (if (= page (- total 1))
      (question-state-complete state)
      (question-state-next state)))

(define (remaining-time deadline clock)
  (and deadline (- deadline (clock))))

(define (timeout-result state auto-resolve?)
  (let ((resolved (and auto-resolve?
                       (question-state-resolve-recommended state))))
    (if resolved
        (make-question-result 'answered (question-state-complete resolved))
        (make-question-result 'timed-out #f))))

(define (read-other reader question message deadline clock)
  (let ((remaining (remaining-time deadline clock)))
    (and (or (not remaining) (> remaining 0))
         (reader (string-append (single-question-prompt question) " — Other")
                 '() #f (lambda (input) (not (string-null? input)))
                 #:selection-mode 'text
                 #:input-enabled? #t
                 #:message message
                 #:timeout remaining))))

(define (read-comment reader question option message deadline clock)
  (let ((remaining (remaining-time deadline clock)))
    (and (or (not remaining) (> remaining 0))
         (reader (string-append (single-question-prompt question) " — "
                                (question-option-label option) " — Comment")
                 '() #f (lambda (input) (not (string-null? input)))
                 #:selection-mode 'text
                 #:input-enabled? #t
                 #:message
                 (string-append
                  "Add context to the selected choice  ·  Enter attach  ·  Esc back\n"
                  message)
                 #:timeout remaining))))

(define* (ask-questions/result questions
                        #:key (reader graphical-reader) (timeout #f)
                        (clock monotonic-seconds) (auto-resolve? #f)
                        (context #f))
  "Display QUESTIONS and return a structured termination result.
READER defaults to `completing-read'; injecting it is useful for embedding and
headless verification."
  (unless (or (not timeout)
              (and (real? timeout) (> timeout 0)))
    (error "question timeout must be a positive real number or #f" timeout))
  (unless (procedure? clock)
    (error "question clock must be a procedure" clock))
  (unless (boolean? auto-resolve?)
    (error "question automatic-resolution marker must be boolean"
           auto-resolve?))
  (unless (or (not context) (string? context))
    (error "question context must be a string or #f" context))
  (let ((deadline (and timeout (+ (clock) timeout))))
   (let loop ((state (make-question-state questions)))
    (let* ((question (question-state-current-question state))
           (page (question-state-page state))
           (options (single-question-options question))
           (allow-other? (single-question-allow-other? question))
           (other-index (and allow-other? (length options)))
           (back-index (+ (length options) (if allow-other? 1 0)))
           (choices (append (map option-display options)
                            (if allow-other? (list %other-label) '())
                            (if (positive? page) (list %back-label) '())))
           (option-details
            (append (map question-option-description options)
                    (if allow-other? (list #f) '())
                    (if (positive? page) (list #f) '())))
           (message (question-message
                     question page (length questions) context))
           (remaining (remaining-time deadline clock))
           (raw-answer
            (and (or (not remaining) (> remaining 0))
                 (reader (single-question-prompt question) choices
                         #:selection-mode 'menu-index
                         #:input-enabled? #f
                         #:option-details option-details
                         #:comment-on-tab? #t
                         #:initial-selected-index
                         (initial-choice-index state question options)
                         #:message message
                         #:timeout remaining))))
      (call-with-values
          (lambda ()
            (if raw-answer
                (reader-result raw-answer)
                (values (if (and remaining (<= remaining 0))
                            'timed-out
                            'cancelled)
                        #f)))
        (lambda (status answer)
         (cond
       ((eq? status 'timed-out) (timeout-result state auto-resolve?))
       ((not (eq? status 'answered)) (make-question-result status #f))
       ((and (pair? answer) (eq? (car answer) 'comment))
        (let ((index (cadr answer)))
          (unless (and (integer? index) (<= 0 index) (< index (length options)))
            (error "graphical question returned an invalid comment index" index))
          (let ((option (list-ref options index)))
            (let ((selected (question-state-select
                             state (question-option-id option))))
             (call-with-values
                (lambda ()
                  (reader-result
                   (read-comment reader question option message deadline clock)))
              (lambda (comment-status text)
                (cond
                 ((eq? comment-status 'timed-out)
                  (timeout-result state auto-resolve?))
                 ((eq? comment-status 'cancelled)
                  (loop selected))
                 ((not (eq? comment-status 'answered))
                  (make-question-result comment-status #f))
                 (else
                  (let* ((answered
                          (question-state-answer-comment
                           selected (question-option-id option) text))
                         (next (advance-or-complete
                                answered page (length questions))))
                    (if (question-state? next)
                        (loop next)
                        (make-question-result 'answered next)))))))))))
       ((not (and (integer? answer) (exact? answer)
                  (<= 0 answer) (< answer (length choices))))
        (error "graphical question returned an invalid choice index" answer))
       ((and (positive? page) (= answer back-index))
        (loop (question-state-back state)))
       ((and allow-other? (= answer other-index))
        (call-with-values
            (lambda ()
              (reader-result
               (read-other reader question message deadline clock)))
          (lambda (other-status text)
            (cond
             ((or (eq? other-status 'timed-out)
                  (and deadline
                       (<= (remaining-time deadline clock) 0)))
              (timeout-result state auto-resolve?))
             ((not (eq? other-status 'answered))
              (make-question-result other-status #f))
             (else
              (let ((answered (question-state-answer-other state text)))
                (let ((next (advance-or-complete
                             answered page (length questions))))
                  (if (question-state? next)
                      (loop next)
                      (make-question-result 'answered next)))))))))
       (else
        (let* ((option (list-ref options answer))
               (selected (question-state-select
                          state (question-option-id option))))
          (let ((next (advance-or-complete
                       selected page (length questions))))
            (if (question-state? next)
                (loop next)
                (make-question-result 'answered next))))))))))))

(define (ask-questions questions . arguments)
  "Compatibility wrapper returning answers, or #f on termination."
  (let ((result (apply ask-questions/result questions arguments)))
    (and (eq? (question-result-status result) 'answered)
         (question-result-answers result))))

(define* (confirm/result prompt
                         #:key (reader graphical-reader) (timeout #f)
                         (clock monotonic-seconds) (auto-resolve? #f)
                         (recommended #f)
                         (yes-label "Yes") (no-label "No")
                         (yes-description #f) (no-description #f))
  "Ask a yes/no question and return a structured termination result.
On success, the result answers are `((confirmation . yes))' or
`((confirmation . no))'.  RECOMMENDED may be `yes', `no', or #f."
  (unless (memq recommended '(#f yes no))
    (error "confirmation recommendation must be yes, no, or #f" recommended))
  (ask-questions/result
   (list
    (make-single-question
     'confirmation prompt
     (list (make-question-option
            'yes yes-label #:description yes-description
            #:recommended? (eq? recommended 'yes))
           (make-question-option
            'no no-label #:description no-description
            #:recommended? (eq? recommended 'no)))))
   #:reader reader #:timeout timeout #:clock clock
   #:auto-resolve? auto-resolve?))

(define (confirm prompt . arguments)
  "Ask a yes/no question, returning #t only when Yes is selected.
Use `confirm/result' when No must remain distinct from termination."
  (let ((result (apply confirm/result prompt arguments)))
    (and (eq? (question-result-status result) 'answered)
         (eq? (assq-ref (question-result-answers result) 'confirmation)
              'yes))))
