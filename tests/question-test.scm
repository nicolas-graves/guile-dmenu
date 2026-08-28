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
(define other-question
  (make-single-question 'details "Details?" (list safe fast)
                        #:allow-other? #t))
(test-assert "Other answers are opt-in"
  (single-question-allow-other? other-question))
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

(let* ((state (make-question-state (list other-question)))
       (answered (question-state-answer-other state "custom details")))
  (test-assert "Other answer is retained as a distinct value"
    (question-other-answer?
     (question-state-selected-option answered)))
(test-equal "Other completion is tagged independently of option ids"
    '((details other . "custom details"))
    (question-state-complete answered)))

(let* ((state (make-question-state (list other-question)))
       (answered (question-state-answer-comment state 'safe "Please inspect first")))
  (test-equal "choice comments retain both the option and free-form text"
    '((details choice safe comment "Please inspect first"))
    (question-state-complete answered)))

(test-error "Other answer requires question opt-in" #t
  (question-state-answer-other (make-question-state (list question)) "text"))
(test-error "Other answer cannot be empty" #t
  (question-state-answer-other
   (make-question-state (list other-question)) ""))

(define format-question
  (make-single-question 'format "Output format?"
                        (list (make-question-option 'text "Text")
                              (make-question-option 'json "JSON"))))
(define publish-question
  (make-single-question 'publish "Publish now?"
                        (list (make-question-option 'yes "Yes")
                              (make-question-option 'no "No"))))
(define paged (make-question-state
               (list question format-question publish-question)))

(test-equal "batch starts on its first question" 'mode
  (single-question-id (question-state-current-question paged)))
(test-equal "batch accepts three questions" 3
  (length (question-state-questions paged)))
(test-assert "batch accepts one question"
  (question-state? (make-question-state (list question))))
(test-assert "batch accepts two questions"
  (question-state? (make-question-state (list question format-question))))
(test-error "empty batches are rejected" #t (make-question-state '()))
(test-error "batches longer than three are rejected" #t
  (make-question-state (list question format-question publish-question question)))
(test-error "duplicate question ids are rejected" #t
  (make-question-state (list question question)))

(define page-one (question-state-select paged 'fast))
(define page-two (question-state-next page-one))
(define page-two-answered (question-state-select page-two 'json))
(define back-on-one (question-state-back page-two-answered))
(test-eq "Back retains the first answer" fast
  (question-state-selected-option back-on-one))
(define forward-on-two (question-state-next back-on-one))
(test-equal "Next retains the later answer" 'json
  (question-option-id (question-state-selected-option forward-on-two)))
(define page-three
  (question-state-next forward-on-two))
(define complete-state (question-state-select page-three 'no))
(test-equal "completion returns every id-to-answer pair"
  '((mode . fast) (format . json) (publish . no))
  (question-state-complete complete-state))
(test-error "Next requires an answer" #t (question-state-next paged))
(test-error "completion requires every answer" #t
  (question-state-complete page-one))
(test-eqv "cancellation returns #f on first page" #f
  (question-state-cancel paged))
(test-eqv "cancellation returns #f on middle page" #f
  (question-state-cancel page-two))
(test-eqv "cancellation returns #f on last page" #f
  (question-state-cancel page-three))
(test-error "Back rejects the first page" #t (question-state-back paged))
(test-error "Next rejects the final page" #t
  (question-state-next complete-state))

(test-end "single-question state")
(primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1))
