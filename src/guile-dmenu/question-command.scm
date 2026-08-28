(define-module (guile-dmenu question-command)
  #:use-module (guile-dmenu question)
  #:use-module (guile-dmenu structured-prompt)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (read-question-request
            question-result->json
            run-question-command))

(define (field object name)
  (let ((entry (and (list? object)
                    (or (assoc name object)
                        (assoc (string->symbol name) object)))))
    (and entry (cdr entry))))

(define (required-field object name)
  (or (field object name)
      (error "missing required JSON question field" name)))

(define (sequence->list value name)
  (cond ((vector? value) (vector->list value))
        ((list? value) value)
        (else (error "JSON question field must be an array" name))))

(define (option-from-json object)
  (make-question-option
   (required-field object "id")
   (required-field object "label")
   #:description (field object "description")
   #:recommended? (or (field object "recommended") #f)))

(define (question-from-json object)
  (make-single-question
   (required-field object "id")
   (required-field object "prompt")
   (map option-from-json
        (sequence->list (required-field object "options") "options"))
   #:allow-other? (or (field object "allowOther") #f)))

(define (read-question-request port)
  "Read and validate one structured question request from PORT.
Return QUESTIONS, TIMEOUT, and AUTO-RESOLVE? as three values."
  (let* ((request (json->scm port))
         (questions
          (map question-from-json
               (sequence->list (required-field request "questions")
                               "questions")))
         (timeout (field request "timeout"))
         (auto-resolve? (or (field request "autoResolve") #f)))
    (let trailing ()
      (let ((character (read-char port)))
        (unless (eof-object? character)
          (if (char-whitespace? character)
              (trailing)
              (error "trailing data after JSON question request")))))
    (values questions timeout auto-resolve?)))

(define (json-answer entry)
  (let ((answer (cdr entry)))
    (list
     (cons 'id (car entry))
     (cons 'answer
           (if (question-other-answer? answer)
               (list (cons 'other (question-other-answer-text answer)))
               answer)))))

(define (question-result->json result)
  `((status . ,(symbol->string (question-result-status result)))
    (answers . ,(if (question-result-answers result)
                    (list->vector
                     (map json-answer (question-result-answers result)))
                    #f))))

(define* (run-question-command input output #:key reader)
  "Consume one JSON request from INPUT and write one JSON outcome to OUTPUT."
  (call-with-values
      (lambda () (read-question-request input))
    (lambda (questions timeout auto-resolve?)
      (let ((arguments
             (append (list questions #:timeout timeout
                           #:auto-resolve? auto-resolve?)
                     (if reader (list #:reader reader) '()))))
        (scm->json (question-result->json
                    (apply ask-questions/result arguments))
                   output)
        (newline output)))))
