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
         (cwd (first-ref request '(cwd working_directory workingDirectory)))
         (project
          (or (first-ref request '(project project_name projectName))
              (and (string? cwd)
                   (let ((parts (remove string-null? (string-split cwd #\/))))
                     (and (pair? parts) (last parts))))))
         (reason
          (or (first-ref request '(reason description))
              (and (list? input)
                   (first-ref input '(reason description justification)))))
         (command (or (and (list? input)
                           (first-ref input '(command cmd path url)))
                      input)))
    (string-join
     (list (string-append "Project: "
                          (display-value project))
           (string-append "Working directory: "
                          (display-value cwd))
           (string-append "Reason: "
                          (display-value reason))
           (string-append "Requested command: " (display-value command)))
     "\n")))

(define (decision behavior . message)
  `((hookSpecificOutput
     . ((hookEventName . "PermissionRequest")
        (decision . ,(append `((behavior . ,behavior))
                             (if (null? message) '()
                                 `((message . ,(car message))))))))))
(define* (allow-result #:optional system-message)
  (append (decision "allow")
          (if system-message
              `((systemMessage . ,system-message))
              '())))
(define* (deny-result
          #:optional (message "Denied from graphical approval prompt."))
  (decision "deny" message))
