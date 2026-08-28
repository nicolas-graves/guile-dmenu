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
            single-question-allow-other?
            question-other-answer?
            question-other-answer-text
            make-single-question-state
            single-question-state?
            single-question-state-question
            single-question-state-selected-option
            single-question-select
            single-question-complete
            single-question-cancel
            make-question-state
            question-state?
            question-state-questions
            question-state-page
            question-state-current-question
            question-state-selected-option
            question-state-select
            question-state-answer-other
            question-state-resolve-recommended
            question-state-back
            question-state-next
            question-state-complete
            question-state-cancel))

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
  (%make-single-question id prompt options allow-other?)
  single-question?
  (id single-question-id)
  (prompt single-question-prompt)
  (options single-question-options)
  (allow-other? single-question-allow-other?))

(define-record-type <question-other-answer>
  (%make-question-other-answer text)
  question-other-answer?
  (text question-other-answer-text))

(define (duplicate-option-id options)
  (find (lambda (option)
          (> (count (lambda (other)
                      (equal? (question-option-id option)
                              (question-option-id other)))
                    options)
             1))
        options))

(define* (make-single-question id prompt options #:key (allow-other? #f))
  (unless (or (symbol? id) (and (string? id) (not (string-null? id))))
    (error "question id must be a symbol or nonempty string" id))
  (unless (and (string? prompt) (not (string-null? prompt)))
    (error "question prompt must be a nonempty string" prompt))
  (unless (and (list? options) (every question-option? options))
    (error "question options must be a list of question options" options))
  (unless (boolean? allow-other?)
    (error "question allow-other marker must be boolean" allow-other?))
  (unless (memv (length options) '(2 3))
    (error "a single question requires two or three options" options))
  (let ((duplicate (duplicate-option-id options)))
    (when duplicate
      (error "question option ids must be unique"
             (question-option-id duplicate))))
  (when (> (count question-option-recommended? options) 1)
    (error "a question may have at most one recommended option" options))
  (%make-single-question id prompt options allow-other?))

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

;; A question state owns all structured-question navigation state.  In
;; particular, none of this state is shared with the raw string-list menu.
(define-record-type <question-state>
  (%make-question-state questions page answers)
  question-state?
  (questions question-state-questions)
  (page question-state-page)
  (answers question-state-answers))

(define (duplicate-question-id questions)
  (find (lambda (question)
          (> (count (lambda (other)
                      (equal? (single-question-id question)
                              (single-question-id other)))
                    questions)
             1))
        questions))

(define (make-question-state questions)
  (unless (and (list? questions) (every single-question? questions))
    (error "questions must be a list of questions" questions))
  (unless (memv (length questions) '(1 2 3))
    (error "a question state requires one to three questions" questions))
  (let ((duplicate (duplicate-question-id questions)))
    (when duplicate
      (error "question ids must be unique" (single-question-id duplicate))))
  (%make-question-state questions 0 (make-list (length questions) #f)))

(define (question-state-current-question state)
  (unless (question-state? state)
    (error "expected a question state" state))
  (list-ref (question-state-questions state) (question-state-page state)))

(define (question-state-selected-option state)
  (unless (question-state? state)
    (error "expected a question state" state))
  (list-ref (question-state-answers state) (question-state-page state)))

(define (replace-at items index value)
  (let loop ((rest items) (position 0))
    (cond ((null? rest) '())
          ((= position index) (cons value (cdr rest)))
          (else (cons (car rest) (loop (cdr rest) (+ position 1)))))))

(define (question-state-select state option-id)
  (unless (question-state? state)
    (error "expected a question state" state))
  (let* ((question (question-state-current-question state))
         (option (find (lambda (candidate)
                         (equal? option-id (question-option-id candidate)))
                       (single-question-options question))))
    (unless option
      (error "unknown question option id" option-id))
    (%make-question-state
     (question-state-questions state)
     (question-state-page state)
     (replace-at (question-state-answers state)
                 (question-state-page state) option))))

(define (question-state-answer-other state text)
  (unless (question-state? state)
    (error "expected a question state" state))
  (unless (single-question-allow-other?
           (question-state-current-question state))
    (error "current question does not allow an Other answer"))
  (unless (and (string? text) (not (string-null? text)))
    (error "Other answer must be a nonempty string" text))
  (%make-question-state
   (question-state-questions state)
   (question-state-page state)
   (replace-at (question-state-answers state)
               (question-state-page state)
               (%make-question-other-answer text))))

(define (question-state-resolve-recommended state)
  "Fill unanswered questions from their recommended options, or return #f.
Answers already supplied by the user are retained."
  (unless (question-state? state)
    (error "expected a question state" state))
  (let loop ((questions (question-state-questions state))
             (answers (question-state-answers state))
             (resolved '()))
    (cond
     ((null? questions)
      (%make-question-state (question-state-questions state)
                            (question-state-page state)
                            (reverse resolved)))
     ((car answers)
      (loop (cdr questions) (cdr answers) (cons (car answers) resolved)))
     (else
      (let ((recommended
             (find question-option-recommended?
                   (single-question-options (car questions)))))
        (and recommended
             (loop (cdr questions) (cdr answers)
                   (cons recommended resolved))))))))

(define (question-state-back state)
  (unless (question-state? state)
    (error "expected a question state" state))
  (when (zero? (question-state-page state))
    (error "already at the first question"))
  (%make-question-state (question-state-questions state)
                        (- (question-state-page state) 1)
                        (question-state-answers state)))

(define (question-state-next state)
  (unless (question-state? state)
    (error "expected a question state" state))
  (unless (question-state-selected-option state)
    (error "cannot leave an unanswered question"
           (single-question-id (question-state-current-question state))))
  (when (= (+ (question-state-page state) 1)
           (length (question-state-questions state)))
    (error "already at the last question"))
  (%make-question-state (question-state-questions state)
                        (+ (question-state-page state) 1)
                        (question-state-answers state)))

(define (question-state-complete state)
  (unless (question-state? state)
    (error "expected a question state" state))
  (unless (every (lambda (answer)
                   (or (question-option? answer)
                       (question-other-answer? answer)))
                 (question-state-answers state))
    (error "cannot complete with unanswered questions"))
  (map (lambda (question answer)
         (cons (single-question-id question)
               (if (question-option? answer)
                   (question-option-id answer)
                   (cons 'other (question-other-answer-text answer)))))
       (question-state-questions state)
       (question-state-answers state)))

(define (question-state-cancel state)
  (unless (question-state? state)
    (error "expected a question state" state))
  #f)
