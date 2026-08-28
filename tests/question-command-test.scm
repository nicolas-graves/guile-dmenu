(use-modules (guile-dmenu question)
             (guile-dmenu question-command)
             (json)
             (srfi srfi-64))

(define request
  "{\"questions\":[{\"id\":\"mode\",\"prompt\":\"Mode?\",\"options\":[{\"id\":\"safe\",\"label\":\"Safe\",\"description\":\"Check first\",\"recommended\":true},{\"id\":\"fast\",\"label\":\"Fast\"}]}],\"timeout\":12,\"autoResolve\":true}")

(test-begin "JSON question command")
(call-with-values
    (lambda () (read-question-request (open-input-string request)))
  (lambda (questions timeout auto-resolve?)
    (test-equal "request contains one question" 1 (length questions))
    (test-equal "request retains stable ids" "mode"
      (single-question-id (car questions)))
    (test-equal "request retains timeout" 12 timeout)
    (test-assert "request retains automatic resolution" auto-resolve?)))

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

(test-error "missing questions are rejected" #t
  (read-question-request (open-input-string "{}")))
(test-error "a second stdin payload is rejected" #t
  (read-question-request (open-input-string (string-append request "{}"))))
(test-end "JSON question command")
