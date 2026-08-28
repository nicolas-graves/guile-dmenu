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
                   message timeout option-details comment-on-tab?)
    (set! calls
          (append calls
                  (list (list prompt choices selection-mode input-enabled?
                              initial-selected-index message option-details
                              comment-on-tab?))))
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
  (test-assert "adapter keeps descriptions with their option rows"
    (string-contains (car (cadr (car calls))) "Check first"))
  (test-equal "adapter supplies descriptions as expandable option details"
    '("Check first" #f) (list-ref (car calls) 6))
  (test-assert "adapter enables the Codex-style Tab comment workflow"
    (list-ref (car calls) 7))
  (test-assert "adapter does not repeat descriptions in context"
    (not (string-contains (list-ref (car calls) 5) "Check first")))
  (let ((captured-message #f))
    (ask-questions
     (list first)
     #:context "Working directory: /tmp/project"
     #:reader (lambda* (prompt choices
                        #:key selection-mode input-enabled?
                        initial-selected-index message timeout option-details
                        comment-on-tab?)
                (set! captured-message message)
                0))
    (test-assert "adapter exposes caller context in the graphical message"
      (string-contains captured-message "Working directory: /tmp/project"))
    (test-assert "adapter includes concise navigation help"
      (string-contains captured-message "Tab add comment")))
  (test-error "adapter rejects non-string context" #t
    (ask-questions (list first) #:reader (lambda args 0) #:context '(bad)))
  (test-eqv "graphical cancellation stays distinct" #f
    (ask-questions (list first) #:reader (lambda args #f)))

  (let ((replies '((comment 0) "Check permissions before proceeding")))
    (test-equal "TAB attaches free-form context to the selected choice"
      '((mode choice safe comment "Check permissions before proceeding"))
      (ask-questions
       (list first)
       #:reader
       (lambda args
         (let ((reply (car replies)))
           (set! replies (cdr replies))
           reply)))))

  (let ((result (ask-questions/result
                 (list first) #:reader (lambda args 1))))
    (test-eq "structured success reports answered"
      'answered (question-result-status result))
    (test-equal "structured success retains answers"
      '((mode . fast)) (question-result-answers result)))

  (for-each
   (lambda (status)
     (let ((result
            (ask-questions/result
             (list first)
             #:reader (lambda args (list 'reader-result status #f)))))
       (test-eq (string-append "structured outcome reports "
                               (symbol->string status))
         status (question-result-status result))
       (test-eqv "terminated outcome has no answers"
         #f (question-result-answers result))))
   '(cancelled timed-out window-closed graphical-failure))

  (let ((replies '(0 2 0 1))
        (initial-indices '()))
    (define* (back-reader prompt choices
                          #:key selection-mode input-enabled?
                          initial-selected-index message timeout option-details
                          comment-on-tab?)
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
                    message timeout option-details comment-on-tab?)
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
                    initial-selected-index message timeout option-details
                    comment-on-tab?)
            (set! modes (append modes (list selection-mode)))
            (let ((answer (car replies)))
              (set! replies (cdr replies))
              answer))))
    (test-equal "Other selection returns a tagged free-form answer"
      '((details other . "custom details"))
      (ask-questions (list other-question) #:reader other-reader))
    (test-equal "Other switches from indexed choice to text entry"
      '(menu-index text) modes))

  (let* ((question
          (make-single-question
           'details "Details?"
           (list (make-question-option 'brief "Brief")
                 (make-question-option 'full "Full"))
           #:allow-other? #t))
         (replies '(2 (reader-result answered "graphical details")))
         (reader
          (lambda args
            (let ((answer (car replies)))
              (set! replies (cdr replies))
              answer))))
    (test-equal "Other accepts the real graphical reader result"
      '((details other . "graphical details"))
      (ask-questions (list question) #:reader reader)))

  (let ((times '(10 11 13))
        (replies '(0 1))
        (timeouts '()))
    (define (clock)
      (let ((value (car times)))
        (set! times (cdr times))
        value))
    (define* (timed-reader prompt choices
                           #:key selection-mode input-enabled?
                           initial-selected-index message timeout option-details
                           comment-on-tab?)
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
                              initial-selected-index message timeout option-details
                              comment-on-tab?)
      (set! reader-calls (+ reader-calls 1))
      0)
    (test-eqv "deadline expiration cancels before opening another page" #f
      (ask-questions (list first second) #:reader expiring-reader
                     #:timeout 5 #:clock clock))
    (test-equal "expired batch does not invoke the reader again"
      1 reader-calls))

  (let ((times '(0 6)))
    (define (clock)
      (let ((value (car times)))
        (set! times (cdr times))
        value))
    (let ((result (ask-questions/result
                   (list first) #:reader (lambda args 0)
                   #:timeout 5 #:clock clock)))
      (test-eq "deadline exhaustion is reported as timeout"
        'timed-out (question-result-status result))))

  (let ((times '(0 1 6)))
    (define (clock)
      (let ((value (car times)))
        (set! times (cdr times))
        value))
    (let ((result (ask-questions/result
                   (list first
                         (make-single-question
                          'publish "Publish?"
                          (list (make-question-option 'yes "Yes")
                                (make-question-option
                                 'no "No" #:recommended? #t))))
                   #:reader (lambda args 0)
                   #:timeout 5 #:clock clock #:auto-resolve? #t)))
      (test-eq "opt-in timeout resolution completes the batch"
        'answered (question-result-status result))
      (test-equal "automatic resolution preserves answers and uses recommendations"
        '((mode . safe) (publish . no))
        (question-result-answers result))))

  (let ((result
         (ask-questions/result
          (list second) #:reader (lambda args '(reader-result timed-out #f))
          #:auto-resolve? #t)))
    (test-eq "automatic resolution requires a recommendation"
      'timed-out (question-result-status result)))

  (test-error "automatic-resolution marker must be boolean" #t
    (ask-questions/result (list first) #:reader (lambda args #f)
                          #:auto-resolve? 'yes))

  (test-error "zero batch timeout is rejected" #t
    (ask-questions (list first) #:reader (lambda args #f) #:timeout 0))
  (test-error "non-numeric batch timeout is rejected" #t
    (ask-questions (list first) #:reader (lambda args #f) #:timeout "soon"))

  (let ((result (confirm/result "Continue?" #:reader (lambda args 0))))
    (test-eq "confirmation success uses structured outcomes"
      'answered (question-result-status result))
    (test-equal "confirmation returns a stable yes answer"
      '((confirmation . yes)) (question-result-answers result)))
  (test-assert "confirmation convenience returns true for yes"
    (confirm "Continue?" #:reader (lambda args 0)))
  (test-assert "confirmation convenience returns false for no"
    (not (confirm "Continue?" #:reader (lambda args 1))))
  (let ((result
         (confirm/result
          "Continue?"
          #:reader (lambda args '(reader-result timed-out #f))
          #:recommended 'no #:auto-resolve? #t)))
    (test-eq "confirmation can safely resolve a recommended timeout"
      'answered (question-result-status result))
    (test-equal "recommended confirmation timeout retains an explicit denial"
      '((confirmation . no)) (question-result-answers result)))
  (test-error "confirmation rejects an invalid recommendation" #t
    (confirm/result "Continue?" #:reader (lambda args 0)
                    #:recommended 'maybe))
  (test-end "graphical structured prompt adapter")
  (primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1)))
