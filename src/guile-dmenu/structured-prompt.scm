(define-module (guile-dmenu structured-prompt)
  #:use-module (guile-dmenu question)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (ask-questions))

(define %back-label "← Back")

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

(define* (ask-questions questions #:key (reader graphical-reader) (timeout #f))
  "Display QUESTIONS as real graphical menu sessions and return id answers.
READER defaults to `completing-read'; injecting it is useful for embedding and
headless verification.  Cancellation from any page returns #f."
  (let loop ((state (make-question-state questions)))
    (let* ((question (question-state-current-question state))
           (page (question-state-page state))
           (options (single-question-options question))
           (choices (append (map option-display options)
                            (if (positive? page) (list %back-label) '())))
           (answer (reader (single-question-prompt question) choices
                           #:selection-mode 'menu-index
                           #:input-enabled? #f
                           #:message (question-message
                                      question page (length questions))
                           #:timeout timeout)))
      (cond
       ((not answer) (question-state-cancel state))
       ((not (and (integer? answer) (exact? answer)
                  (<= 0 answer) (< answer (length choices))))
        (error "graphical question returned an invalid choice index" answer))
       ((= answer (length options))
        (loop (question-state-back state)))
       (else
        (let* ((option (list-ref options answer))
               (selected (question-state-select
                          state (question-option-id option))))
          (if (= page (- (length questions) 1))
              (question-state-complete selected)
              (loop (question-state-next selected)))))))))
