(use-modules (guile-dmenu question)
             (srfi srfi-64))

(define runner (test-runner-simple))
(test-runner-current runner)
(test-begin "single-question state")

(define safe
  (make-question-option 'safe "Safe mode"
                        #:description "Use conservative defaults."
                        #:recommended? #t))
(define fast (make-question-option 'fast "Fast mode"))
(define custom
  (make-question-option 'custom "Custom mode"
                        #:description "Choose advanced defaults."))
(define question
  (make-single-question 'mode "How should this run?"
                        (list safe fast custom)))

(test-assert "a valid question is constructed" (single-question? question))
(test-equal "question retains its prompt"
  "How should this run?" (single-question-prompt question))
(test-equal "question accepts three labeled options"
  '("Safe mode" "Fast mode" "Custom mode")
  (map question-option-label (single-question-options question)))
(test-equal "option description is retained"
  "Use conservative defaults." (question-option-description safe))
(test-assert "recommended marker is retained"
  (question-option-recommended? safe))
(test-assert "recommended marker defaults to false"
  (not (question-option-recommended? fast)))

(define initial (make-single-question-state question))
(define selected (single-question-select initial 'fast))

(test-assert "new state starts without a selection"
  (not (single-question-state-selected-option initial)))
(test-eq "selection records the matching option" fast
  (single-question-state-selected-option selected))
(test-equal "completion returns an id-to-answer result"
  '((mode . fast)) (single-question-complete selected))
(test-eqv "cancellation returns #f" #f
  (single-question-cancel selected))

(test-assert "two options are valid"
  (single-question?
   (make-single-question "confirm" "Continue?" (list safe fast))))
(test-error "fewer than two options are rejected" #t
  (make-single-question 'bad "Bad?" (list safe)))
(test-error "more than three options are rejected" #t
  (make-single-question 'bad "Bad?" (list safe fast custom
                                           (make-question-option 'four "Four"))))
(test-error "duplicate option ids are rejected" #t
  (make-single-question 'bad "Bad?"
                        (list safe (make-question-option 'safe "Again"))))
(test-error "multiple recommendations are rejected" #t
  (make-single-question
   'bad "Bad?"
   (list safe (make-question-option 'other "Other" #:recommended? #t))))
(test-error "unknown option selection is rejected" #t
  (single-question-select initial 'missing))
(test-error "an unanswered question cannot complete" #t
  (single-question-complete initial))

(test-end "single-question state")
(primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1))
