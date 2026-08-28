(define-module (guile-dmenu structured-prompt)
  #:use-module (guile-dmenu question)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (ask-questions))

(define %back-label "← Back")
(define %other-label "Other…")

(define (monotonic-seconds)
  (/ (get-internal-real-time) internal-time-units-per-second))

(define (graphical-reader . arguments)
  (apply (module-ref (resolve-interface '(guile-dmenu menu)) 'completing-read)
         arguments))

(define (option-display option)
  (string-append
   (question-option-label option)
   (if (question-option-recommended? option) " [Recommended]" "")))

(define (question-message question page total)
  (let ((descriptions
         (filter-map
          (lambda (option)
            (and (question-option-description option)
                 (string-append (question-option-label option) ": "
                                (question-option-description option))))
          (single-question-options question))))
    (string-join
     (cons (format #f "Question ~a of ~a" (+ page 1) total) descriptions)
     "\n")))

(define (option-index options selected)
  (and (question-option? selected)
       (list-index
        (lambda (option)
          (equal? (question-option-id option)
                  (question-option-id selected)))
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

(define (read-other reader question message deadline clock)
  (let ((remaining (remaining-time deadline clock)))
    (and (or (not remaining) (> remaining 0))
         (reader (string-append (single-question-prompt question) " — Other")
                 '() #f (lambda (input) (not (string-null? input)))
                 #:selection-mode 'text
                 #:input-enabled? #t
                 #:message message
                 #:timeout remaining))))

(define* (ask-questions questions
                        #:key (reader graphical-reader) (timeout #f)
                        (clock monotonic-seconds))
  "Display QUESTIONS as real graphical menu sessions and return id answers.
READER defaults to `completing-read'; injecting it is useful for embedding and
headless verification.  Cancellation from any page returns #f."
  (unless (or (not timeout)
              (and (real? timeout) (> timeout 0)))
    (error "question timeout must be a positive real number or #f" timeout))
  (unless (procedure? clock)
    (error "question clock must be a procedure" clock))
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
           (message (question-message
                     question page (length questions)))
           (remaining (remaining-time deadline clock))
           (answer
            (and (or (not remaining) (> remaining 0))
                 (reader (single-question-prompt question) choices
                         #:selection-mode 'menu-index
                         #:input-enabled? #f
                         #:initial-selected-index
                         (initial-choice-index state question options)
                         #:message message
                         #:timeout remaining))))
      (cond
       ((not answer) (question-state-cancel state))
       ((not (and (integer? answer) (exact? answer)
                  (<= 0 answer) (< answer (length choices))))
        (error "graphical question returned an invalid choice index" answer))
       ((and (positive? page) (= answer back-index))
        (loop (question-state-back state)))
       ((and allow-other? (= answer other-index))
        (let ((text (read-other reader question message deadline clock)))
          (if (not text)
              (question-state-cancel state)
              (let ((answered (question-state-answer-other state text)))
                (let ((next (advance-or-complete
                             answered page (length questions))))
                  (if (question-state? next) (loop next) next))))))
       (else
        (let* ((option (list-ref options answer))
               (selected (question-state-select
                          state (question-option-id option))))
          (let ((next (advance-or-complete
                       selected page (length questions))))
            (if (question-state? next) (loop next) next)))))))))
