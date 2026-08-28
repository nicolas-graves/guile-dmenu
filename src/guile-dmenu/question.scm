(define-module (guile-dmenu question)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-question-option
            question-option?
            question-option-id
            question-option-label
            question-option-description
            question-option-recommended?
            make-single-question
            single-question?
            single-question-id
            single-question-prompt
            single-question-options
            make-single-question-state
            single-question-state?
            single-question-state-question
            single-question-state-selected-option
            single-question-select
            single-question-complete
            single-question-cancel))

;; This module deliberately has no dependency on the menu or Wayland modules.
;; Structured questions are opt-in state, not part of ordinary string-list
;; completion sessions.

(define-record-type <question-option>
  (%make-question-option id label description recommended?)
  question-option?
  (id question-option-id)
  (label question-option-label)
  (description question-option-description)
  (recommended? question-option-recommended?))

(define* (make-question-option id label
                               #:key (description #f) (recommended? #f))
  (unless (or (symbol? id) (and (string? id) (not (string-null? id))))
    (error "question option id must be a symbol or nonempty string" id))
  (unless (and (string? label) (not (string-null? label)))
    (error "question option label must be a nonempty string" label))
  (unless (or (not description) (string? description))
    (error "question option description must be a string or #f" description))
  (unless (boolean? recommended?)
    (error "question option recommended marker must be boolean" recommended?))
  (%make-question-option id label description recommended?))

(define-record-type <single-question>
  (%make-single-question id prompt options)
  single-question?
  (id single-question-id)
  (prompt single-question-prompt)
  (options single-question-options))

(define (duplicate-option-id options)
  (find (lambda (option)
          (> (count (lambda (other)
                      (equal? (question-option-id option)
                              (question-option-id other)))
                    options)
             1))
        options))

(define (make-single-question id prompt options)
  (unless (or (symbol? id) (and (string? id) (not (string-null? id))))
    (error "question id must be a symbol or nonempty string" id))
  (unless (and (string? prompt) (not (string-null? prompt)))
    (error "question prompt must be a nonempty string" prompt))
  (unless (and (list? options) (every question-option? options))
    (error "question options must be a list of question options" options))
  (unless (memv (length options) '(2 3))
    (error "a single question requires two or three options" options))
  (let ((duplicate (duplicate-option-id options)))
    (when duplicate
      (error "question option ids must be unique"
             (question-option-id duplicate))))
  (when (> (count question-option-recommended? options) 1)
    (error "a question may have at most one recommended option" options))
  (%make-single-question id prompt options))

(define-record-type <single-question-state>
  (%make-single-question-state question selected-option)
  single-question-state?
  (question single-question-state-question)
  (selected-option single-question-state-selected-option))

(define (make-single-question-state question)
  (unless (single-question? question)
    (error "expected a single question" question))
  (%make-single-question-state question #f))

(define (single-question-select state option-id)
  (unless (single-question-state? state)
    (error "expected a single-question state" state))
  (let ((option (find (lambda (candidate)
                        (equal? option-id (question-option-id candidate)))
                      (single-question-options
                       (single-question-state-question state)))))
    (unless option
      (error "unknown question option id" option-id))
    (%make-single-question-state
     (single-question-state-question state) option)))

(define (single-question-complete state)
  (unless (single-question-state? state)
    (error "expected a single-question state" state))
  (let ((selected (single-question-state-selected-option state))
        (question (single-question-state-question state)))
    (unless selected
      (error "cannot complete an unanswered question"
             (single-question-id question)))
    (list (cons (single-question-id question)
                (question-option-id selected)))))

(define (single-question-cancel state)
  (unless (single-question-state? state)
    (error "expected a single-question state" state))
  #f)
