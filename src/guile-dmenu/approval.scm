(define-module (guile-dmenu approval)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (parse-request request-message allow-result deny-result))

(define (ref object key)
  (and (list? object)
       (or (assoc-ref object key)
           (and (symbol? key) (assoc-ref object (symbol->string key))))))
(define (first-ref object keys)
  (let loop ((rest keys))
    (and (pair? rest) (or (ref object (car rest)) (loop (cdr rest))))))
(define (display-value value)
  (cond ((not value) "(not provided)")
        ((string? value) value)
        (else (call-with-output-string (lambda (port) (pretty-print value port))))))

(define (parse-request port) (json->scm port))

(define (request-message request)
  (let* ((input (or (ref request 'tool_input) (ref request 'toolInput)
                    (ref request 'input)))
         (command (or (and (list? input)
                           (first-ref input '(command cmd path url)))
                      input)))
    (string-join
     (list (string-append "Model/session: "
                          (display-value (or (ref request 'model)
                                             (ref request 'session_id)
                                             (ref request 'sessionId))))
           (string-append "Project: "
                          (display-value (first-ref request '(project project_name projectName))))
           (string-append "Working directory: "
                          (display-value (first-ref request '(cwd working_directory workingDirectory))))
           (string-append "Tool: "
                          (display-value (first-ref request '(tool_name toolName tool))))
           (string-append "Reason: "
                          (display-value (first-ref request '(reason description))))
           (string-append "Command/input: " (display-value command)))
     "\n")))

(define (decision behavior . message)
  `((hookSpecificOutput
     . ((hookEventName . "PermissionRequest")
        (decision . ,(append `((behavior . ,behavior))
                             (if (null? message) '()
                                 `((message . ,(car message))))))))))
(define (allow-result) (decision "allow"))
(define (deny-result) (decision "deny" "Denied from graphical approval prompt."))
