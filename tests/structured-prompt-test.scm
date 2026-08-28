(use-modules (guile-dmenu question)
             (guile-dmenu structured-prompt)
             (srfi srfi-64))

(define runner (test-runner-simple))
(test-with-runner runner
  (test-begin "graphical structured prompt adapter")
  (define first
    (make-single-question
     'mode "Mode?"
     (list (make-question-option 'safe "Safe" #:description "Check first")
           (make-question-option 'fast "Fast" #:recommended? #t))))
  (define second
    (make-single-question
     'publish "Publish?"
     (list (make-question-option 'yes "Yes")
           (make-question-option 'no "No"))))
  (define replies (list 1 1))
  (define calls '())
  (define* (reader prompt choices
                   #:key selection-mode input-enabled? initial-selected-index
                   message timeout)
    (set! calls
          (append calls
                  (list (list prompt choices selection-mode input-enabled?
                              initial-selected-index message))))
    (let ((reply (car replies)))
      (set! replies (cdr replies))
      reply))
  (test-equal "real-reader adapter completes every answer"
    '((mode . fast) (publish . no))
    (ask-questions (list first second) #:reader reader))
  (test-equal "adapter opens one menu session per page" 2 (length calls))
  (test-eq "adapter requests index selection"
    'menu-index (list-ref (car calls) 2))
  (test-assert "adapter disables filtering so indices remain stable"
    (not (list-ref (car calls) 3)))
  (test-equal "recommended option is highlighted initially"
    1 (list-ref (car calls) 4))
  (test-assert "adapter exposes descriptions in the graphical message"
    (string-contains (list-ref (car calls) 5) "Safe: Check first"))
  (test-eqv "graphical cancellation stays distinct" #f
    (ask-questions (list first) #:reader (lambda args #f)))

  (let ((replies '(0 2 0 1))
        (initial-indices '()))
    (define* (back-reader prompt choices
                          #:key selection-mode input-enabled?
                          initial-selected-index message timeout)
      (set! initial-indices
            (append initial-indices (list initial-selected-index)))
      (let ((answer (car replies)))
        (set! replies (cdr replies))
        answer))
    (test-equal "Back preserves a non-recommended answer"
      '((mode . safe) (publish . no))
      (ask-questions (list first second) #:reader back-reader))
    (test-equal "retained answer takes precedence over recommendation"
      '(1 0 0 0) initial-indices))

  (let* ((colliding
          (make-single-question
           'collision "Collision?"
           (list (make-question-option 'first "Same")
                 (make-question-option 'second "Same")
                 (make-question-option 'back "← Back"))))
         (answers '(1))
         (index-reader
          (lambda* (prompt choices
                    #:key selection-mode input-enabled? initial-selected-index
                    message timeout)
            (let ((answer (car answers)))
              (set! answers (cdr answers))
              answer))))
    (test-equal "duplicate and navigation-like labels resolve by stable id"
      '((collision . second))
      (ask-questions (list colliding) #:reader index-reader)))

  (test-error "adapter rejects an out-of-range reader result" #t
    (ask-questions (list first) #:reader (lambda args 9)))

  (let* ((other-question
          (make-single-question
           'details "Details?"
           (list (make-question-option 'brief "Brief")
                 (make-question-option 'full "Full"))
           #:allow-other? #t))
         (replies '(2 "custom details"))
         (modes '())
         (other-reader
          (lambda* (prompt choices
                    #:optional predicate require-match
                    #:key selection-mode input-enabled?
                    initial-selected-index message timeout)
            (set! modes (append modes (list selection-mode)))
            (let ((answer (car replies)))
              (set! replies (cdr replies))
              answer))))
    (test-equal "Other selection returns a tagged free-form answer"
      '((details other . "custom details"))
      (ask-questions (list other-question) #:reader other-reader))
    (test-equal "Other switches from indexed choice to text entry"
      '(menu-index text) modes))

  (let ((times '(10 11 13))
        (replies '(0 1))
        (timeouts '()))
    (define (clock)
      (let ((value (car times)))
        (set! times (cdr times))
        value))
    (define* (timed-reader prompt choices
                           #:key selection-mode input-enabled?
                           initial-selected-index message timeout)
      (set! timeouts (append timeouts (list timeout)))
      (let ((answer (car replies)))
        (set! replies (cdr replies))
        answer))
    (test-equal "batch completes under one decreasing deadline"
      '((mode . safe) (publish . no))
      (ask-questions (list first second) #:reader timed-reader
                     #:timeout 10 #:clock clock))
    (test-equal "each page receives only the remaining batch time"
      '(9 7) timeouts))

  (let ((times '(0 1 6))
        (reader-calls 0))
    (define (clock)
      (let ((value (car times)))
        (set! times (cdr times))
        value))
    (define* (expiring-reader prompt choices
                              #:key selection-mode input-enabled?
                              initial-selected-index message timeout)
      (set! reader-calls (+ reader-calls 1))
      0)
    (test-eqv "deadline expiration cancels before opening another page" #f
      (ask-questions (list first second) #:reader expiring-reader
                     #:timeout 5 #:clock clock))
    (test-equal "expired batch does not invoke the reader again"
      1 reader-calls))

  (test-error "zero batch timeout is rejected" #t
    (ask-questions (list first) #:reader (lambda args #f) #:timeout 0))
  (test-error "non-numeric batch timeout is rejected" #t
    (ask-questions (list first) #:reader (lambda args #f) #:timeout "soon"))
  (test-end "graphical structured prompt adapter")
  (primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1)))
