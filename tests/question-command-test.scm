(use-modules (guile-dmenu question)
             (guile-dmenu question-command)
             (json)
             (srfi srfi-64))

(define request
  "{\"questions\":[{\"id\":\"mode\",\"prompt\":\"Mode?\",\"options\":[{\"id\":\"safe\",\"label\":\"Safe\",\"description\":\"Check first\",\"recommended\":true},{\"id\":\"fast\",\"label\":\"Fast\"}]}],\"timeout\":12,\"autoResolve\":true,\"context\":\"Repository: guile-dmenu\"}")

(test-begin "JSON question command")
(call-with-values
    (lambda () (read-question-request (open-input-string request)))
  (lambda (questions timeout auto-resolve? context)
    (test-equal "request contains one question" 1 (length questions))
    (test-equal "request retains stable ids" "mode"
      (single-question-id (car questions)))
    (test-equal "request retains timeout" 12 timeout)
    (test-assert "request retains automatic resolution" auto-resolve?)
    (test-equal "request retains display context"
      "Repository: guile-dmenu" context)))

(let* ((output (open-output-string))
       (reader (lambda args 1)))
  (run-question-command (open-input-string request) output #:reader reader)
  (let ((response (json->scm (open-input-string (get-output-string output)))))
    (test-equal "command emits answered status" "answered"
      (assoc-ref response "status"))
    (test-equal "command emits stable answer id" "mode"
      (assoc-ref (vector-ref (assoc-ref response "answers") 0) "id"))
    (test-equal "command emits selected option id" "fast"
      (assoc-ref (vector-ref (assoc-ref response "answers") 0) "answer"))))

(let* ((other-request
        "{\"questions\":[{\"id\":\"detail\",\"prompt\":\"Detail?\",\"allowOther\":true,\"options\":[{\"id\":\"short\",\"label\":\"Short\"},{\"id\":\"long\",\"label\":\"Long\"}]}]}")
       (replies '(2 "custom detail"))
       (output (open-output-string))
       (reader
        (lambda args
          (let ((answer (car replies)))
            (set! replies (cdr replies))
            answer))))
  (run-question-command (open-input-string other-request) output #:reader reader)
  (let* ((response
          (json->scm (open-input-string (get-output-string output))))
         (answer
          (assoc-ref (vector-ref (assoc-ref response "answers") 0) "answer")))
    (test-equal "command emits tagged Other text" "custom detail"
      (assoc-ref answer "other"))))

(let* ((output (open-output-string))
       (replies '((comment 0) "Use the conservative path"))
       (reader (lambda args
                 (let ((reply (car replies)))
                   (set! replies (cdr replies))
                   reply))))
  (run-question-command (open-input-string request) output #:reader reader)
  (let* ((response (json->scm (open-input-string (get-output-string output))))
         (answer (assoc-ref
                  (vector-ref (assoc-ref response "answers") 0) "answer")))
    (test-equal "comment answer retains selected choice" "safe"
      (assoc-ref answer "choice"))
    (test-equal "comment answer retains free-form context"
      "Use the conservative path" (assoc-ref answer "comment"))))

(test-error "missing questions are rejected" #t
  (read-question-request (open-input-string "{}")))
(test-error "a second stdin payload is rejected" #t
  (read-question-request (open-input-string (string-append request "{}"))))
(test-end "JSON question command")
