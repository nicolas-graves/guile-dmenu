(define-module (guile-dmenu mcp)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:export (mcp-question-handler
            handle-mcp-message
            run-mcp-server))

(define (field object name)
  (let ((entry (and (list? object)
                    (or (assoc name object)
                        (assoc (string->symbol name) object)))))
    (and entry (cdr entry))))

(define (default-question-handler arguments)
  ;; Resolve the graphical modules only for an actual tool call.  This keeps
  ;; MCP initialization and discovery usable in headless environments.
  (let* ((codex (resolve-interface '(guile-dmenu codex)))
         (question-command
          (resolve-interface '(guile-dmenu question-command)))
         (present (module-ref codex 'call-with-codex-presentation))
         (run (module-ref question-command 'run-question-command))
         (input (open-input-string (scm->json-string arguments)))
         (output (open-output-string)))
    (present (lambda () (run input output)))
    (json-string->scm (get-output-string output))))

(define mcp-question-handler (make-parameter default-question-handler))

(define %question-schema
  '((type . "object")
    (properties
     . ((questions
         . ((type . "array")
            (minItems . 1)
            (maxItems . 3)
            (items
             . ((type . "object")
                (required . #("id" "prompt" "options"))
                (properties
                 . ((id . ((type . "string")))
                    (prompt . ((type . "string")))
                    (allowOther . ((type . "boolean")))
                    (options
                     . ((type . "array")
                        (minItems . 2)
                        (maxItems . 3)
                        (items
                         . ((type . "object")
                            (required . #("id" "label"))
                            (properties
                             . ((id . ((type . "string")))
                                (label . ((type . "string")))
                                (description . ((type . "string")))
                                (recommended . ((type . "boolean")))))))))))))))
        (timeout . ((type . "number") (exclusiveMinimum . 0)))
        (autoResolve . ((type . "boolean")))
        (context . ((type . "string")))))
    (required . #("questions"))))

(define %tool
  `((name . "ask_questions")
    (description . "Ask the user one to three short structured questions in guile-dmenu. Use this instead of asking choices in prose.")
    (inputSchema . ,%question-schema)))

(define (result id value)
  `((jsonrpc . "2.0") (id . ,id) (result . ,value)))

(define (error-result id code message)
  `((jsonrpc . "2.0") (id . ,id)
    (error . ((code . ,code) (message . ,message)))))

(define (tool-result answer)
  `((content . #(((type . "text")
                  (text . ,(scm->json-string answer)))))
    (structuredContent . ,answer)
    (isError . #f)))

(define (handle-mcp-message message)
  "Return a JSON-RPC response for MESSAGE, or #f for notifications."
  (let ((id (field message "id"))
        (method (field message "method"))
        (params (field message "params")))
    (cond
     ((equal? method "initialize")
      (result id '((protocolVersion . "2025-06-18")
                   (capabilities . ((tools . ())))
                   (serverInfo . ((name . "guile-dmenu")
                                  (version . "0.1.0"))))))
     ((equal? method "notifications/initialized") #f)
     ((equal? method "ping") (result id '()))
     ((equal? method "tools/list")
      (result id `((tools . #(,%tool)))))
     ((equal? method "tools/call")
      (let ((name (field params "name"))
            (arguments (or (field params "arguments") '())))
        (if (equal? name "ask_questions")
            (catch #t
              (lambda ()
                (result id (tool-result ((mcp-question-handler) arguments))))
              (lambda args
                (result id
                        `((content . #(((type . "text")
                                       (text . "graphical question failed"))))
                          (isError . #t)))))
            (error-result id -32601 "unknown tool"))))
     (id (error-result id -32601 "unknown method"))
     (else #f))))

(define* (run-mcp-server #:optional
                         (input (current-input-port))
                         (output (current-output-port)))
  "Serve newline-delimited MCP JSON-RPC on INPUT and OUTPUT.
Tool calls run on worker threads so a cancelled graphical prompt cannot block
protocol messages or later questions."
  (define output-lock (make-mutex))
  (define workers-lock (make-mutex))
  (define workers (make-hash-table))
  (define (send response)
    (with-mutex output-lock
      (scm->json response output)
      (newline output)
      (force-output output)))
  (define (cancel-worker! id)
    (let ((worker
           (with-mutex workers-lock
             (let ((value (hash-ref workers id #f)))
               (when value (hash-remove! workers id))
               value))))
      (when worker (cancel-thread worker))))
  (define (start-tool-call! id message)
    ;; Hold the registry lock through thread creation and registration.  A
    ;; fast worker's cleanup cannot remove itself until it has been registered.
    (with-mutex workers-lock
      (let ((worker
             (call-with-new-thread
              (lambda ()
                (dynamic-wind
                  (lambda () #t)
                  (lambda ()
                    (let ((response
                           (catch #t
                             (lambda () (handle-mcp-message message))
                             (lambda args
                               (error-result id -32603 "tool call failed")))))
                      (when response (send response))))
                  (lambda ()
                    (with-mutex workers-lock
                      (hash-remove! workers id))))))))
        (hash-set! workers id worker))))
  (let loop ((line (read-line input)))
    (unless (eof-object? line)
      (unless (string-null? line)
        (let ((message
               (catch #t
                 (lambda () (json-string->scm line))
                 (lambda args #f))))
          (if (not message)
              (send (error-result #f -32700 "invalid request"))
              (let ((method (field message "method"))
                    (id (field message "id")))
                (cond
                 ((equal? method "tools/call")
                  (start-tool-call! id message))
                 ((equal? method "notifications/cancelled")
                  (cancel-worker! (field (field message "params")
                                         "requestId")))
                 (else
                  (let ((response (handle-mcp-message message)))
                    (when response (send response)))))))))
      (loop (read-line input))))
  ;; A closed MCP stdin means teardown.  Wait for requests that have already
  ;; reached a terminal response instead of abandoning their worker threads.
  (for-each join-thread
            (with-mutex workers-lock
              (hash-map->list (lambda (id worker) worker) workers))))

