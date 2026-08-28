(use-modules (guile-dmenu question)
             (guile-dmenu structured-prompt)
             (srfi srfi-64))

(define runner (test-runner-simple))
(test-with-runner runner
  (test-begin "graphical structured prompt adapter")
  (define first
    (make-single-question
     'mode "Mode?"
     (list (make-question-option 'fast "Fast" #:recommended? #t)
           (make-question-option 'safe "Safe" #:description "Check first"))))
  (define second
    (make-single-question
     'publish "Publish?"
     (list (make-question-option 'yes "Yes")
           (make-question-option 'no "No"))))
  (define replies (list "Fast [Recommended]" "No"))
  (define calls '())
  (define* (reader prompt choices #:key selection-mode message timeout)
    (set! calls (append calls (list (list prompt choices selection-mode message))))
    (let ((reply (car replies)))
      (set! replies (cdr replies))
      reply))
  (test-equal "real-reader adapter completes every answer"
    '((mode . fast) (publish . no))
    (ask-questions (list first second) #:reader reader))
  (test-equal "adapter opens one menu session per page" 2 (length calls))
  (test-eq "adapter requests menu selection" 'menu (list-ref (car calls) 2))
  (test-assert "adapter exposes descriptions in the graphical message"
    (string-contains (list-ref (car calls) 3) "Safe: Check first"))
  (test-eqv "graphical cancellation stays distinct" #f
    (ask-questions (list first) #:reader (lambda args #f)))
  (test-end "graphical structured prompt adapter")
  (primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1)))
