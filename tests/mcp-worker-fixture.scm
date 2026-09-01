(use-modules (ice-9 rdelim)
             (json))

(define (field object name)
  (let ((entry (or (assoc name object)
                   (assoc (string->symbol name) object))))
    (and entry (cdr entry))))

(let* ((request (json->scm (current-input-port)))
       (mode (field request "context")))
  (cond
   ((equal? mode "sleep") (sleep 30))
   ((equal? mode "echo-timeout")
    (scm->json
     `((status . "answered")
       (answers . ,(vector
                    `((id . "timeout")
                      (answer . ,(number->string
                                   (field request "timeout"))))))))
    (newline))
   ((equal? mode "failure") (primitive-exit 1))
   ((equal? mode "malformed") (display "not-json\n"))
   ((equal? mode "extra")
    (display "{\"status\":\"answered\",\"answers\":[]}\nextra\n"))
   ((equal? mode "timed-out")
    (display "{\"status\":\"timed-out\",\"answers\":false}\n"))
   (else
    (display "{\"status\":\"answered\",\"answers\":[]}\n")))
  (force-output))
