(define-module (guile-dmenu mcp)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (srfi srfi-9)
  #:export (mcp-question-handler mcp-trace-destination handle-mcp-message
            run-question-worker run-mcp-server))

(define %maximum-question-timeout 240)
(define %worker-timeout-cleanup-margin 1)

(define (field object name)
  (let ((entry (and (list? object)
                    (or (assoc name object)
                        (assoc (string->symbol name) object)))))
    (and entry (cdr entry))))

(define (default-question-handler arguments)
  ;; Resolve graphical modules only in a one-shot worker.
  (let* ((codex (resolve-interface '(guile-dmenu codex)))
         (question-command (resolve-interface '(guile-dmenu question-command)))
         (present (module-ref codex 'call-with-codex-presentation))
         (run (module-ref question-command 'run-question-command))
         (input (open-input-string (scm->json-string arguments)))
         (output (open-output-string)))
    (present (lambda () (run input output)))
    (json-string->scm (get-output-string output))))

(define mcp-question-handler (make-parameter default-question-handler))
(define mcp-trace-destination
  (make-parameter (getenv "GUILE_DMENU_MCP_TRACE")))

(define %question-schema
  `((type . "object")
    (properties
     . ((questions
         . ((type . "array") (minItems . 1) (maxItems . 3)
            (items
             . ((type . "object")
                (required . #("id" "prompt" "options"))
                (properties
                 . ((id . ((type . "string")))
                    (prompt . ((type . "string")))
                    (allowOther . ((type . "boolean")))
                    (options
                     . ((type . "array") (minItems . 2) (maxItems . 3)
                        (items
                         . ((type . "object")
                            (required . #("id" "label"))
                            (properties
                             . ((id . ((type . "string")))
                                (label . ((type . "string")))
                                (description . ((type . "string")))
                                (recommended . ((type . "boolean")))))))))))))))
        (timeout . ((type . "number") (exclusiveMinimum . 0)
                    (maximum . ,%maximum-question-timeout)))
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
    (structuredContent . ,answer) (isError . #f)))

(define (tool-error message)
  `((content . #(((type . "text") (text . ,message)))) (isError . #t)))

(define (valid-timeout? arguments)
  (let ((timeout (field arguments "timeout")))
    (or (not timeout)
        (and (real? timeout) (> timeout 0)
             (<= timeout %maximum-question-timeout)))))

(define (valid-question-result? value)
  (let ((status (field value "status")) (answers (field value "answers")))
    (and (member status '("answered" "cancelled" "timed-out"
                          "window-closed" "graphical-failure"))
         (if (equal? status "answered") (vector? answers) (not answers)))))

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
     ((equal? method "tools/list") (result id `((tools . #(,%tool)))))
     ((equal? method "tools/call")
      (let ((name (field params "name"))
            (arguments (or (field params "arguments") '())))
        (if (equal? name "ask_questions")
            (if (not (valid-timeout? arguments))
                (result id (tool-error
                            "question timeout must be between 0 and 240 seconds"))
                (catch #t
                  (lambda ()
                    (result id (tool-result ((mcp-question-handler) arguments))))
                  (lambda args
                    (result id (tool-error "graphical question failed")))))
            (error-result id -32601 "unknown tool"))))
     (id (error-result id -32601 "unknown method"))
     (else #f))))

(define (run-question-worker input output)
  "Consume one question argument object and write one terminal result.
Return an exit status; the command wrapper performs the hard process exit."
  (catch #t
    (lambda ()
      (let* ((payload (json->scm input))
             (request-id (field payload "__mcpRequestId"))
             (arguments (filter (lambda (entry)
                                  (not (member (car entry)
                                               '("__mcpRequestId"
                                                 __mcpRequestId))))
                                payload)))
        (trace-event "child" "request-read" request-id (getpid))
        (let trailing ()
          (let ((character (read-char input)))
            (unless (eof-object? character)
              (if (char-whitespace? character)
                  (trailing)
                  (error "trailing worker input")))))
        (unless (valid-timeout? arguments)
          (error "question timeout exceeds MCP maximum"))
        (trace-event "child" "graphical-start" request-id (getpid))
        (let ((answer ((mcp-question-handler) arguments)))
          (trace-event "child" "graphical-terminal" request-id (getpid)
                       (cons 'status (field answer "status")))
          (scm->json answer output))
        (newline output)
        (force-output output)
        (trace-event "child" "response-flushed" request-id (getpid))
        0))
    (lambda args
      ;; Exception details can contain request payloads, so keep this generic.
      (scm->json '((error . "graphical question failed")) output)
      (newline output)
      (force-output output)
      1)))

(define-record-type <worker>
  (make-worker pid input thread cancelled?)
  worker?
  (pid worker-pid)
  (input worker-input)
  (thread worker-thread set-worker-thread!)
  (cancelled? worker-cancelled? set-worker-cancelled?!))

(define (trace-event role event id pid . fields)
  (let ((destination (mcp-trace-destination)))
    (when (and destination (not (string-null? destination)))
      ;; Diagnostics must never be able to break protocol completion.
      (catch #t
        (lambda ()
          (let* ((data (append `((time . ,(get-internal-real-time))
                                 (role . ,role) (event . ,event)
                                 (requestId . ,id) (pid . ,pid)) fields))
                 (line (scm->json-string data))
                 (port (if (equal? destination "stderr")
                           (current-error-port)
                           (open-file destination "a"))))
            (display line port)
            (newline port)
            (force-output port)
            (unless (equal? destination "stderr") (close-port port))))
        (lambda args #f)))))

(define (spawn-question-worker command id arguments)
  (let* ((argv (if (list? command) command (list command)))
         (program (car argv))
         (arguments* (append (cdr argv) '("--question-worker")))
         ;; Guile's public OPEN_BOTH port hides its two underlying pipes and
         ;; cannot be half-closed.  The implementation helper returns them
         ;; separately, which lets the child observe request EOF immediately.
         (open-process (module-ref (resolve-module '(ice-9 popen))
                                   'open-process)))
    (call-with-values
        (lambda () (apply open-process OPEN_BOTH program arguments*))
      (lambda (input output pid)
        (catch #t
          (lambda ()
            (scm->json (acons '__mcpRequestId id arguments) output)
            (newline output)
            (force-output output)
            (close-port output)
            (make-worker pid input #f #f))
          (lambda exception
            (catch #t (lambda () (close-port output)) (lambda args #f))
            (catch #t (lambda () (close-port input)) (lambda args #f))
            (catch 'system-error (lambda () (kill pid SIGKILL))
              (lambda args #f))
            (catch 'system-error (lambda () (waitpid pid))
              (lambda args #f))
            (apply throw exception)))))))

(define* (run-mcp-server #:optional
                         (input (current-input-port))
                         (output (current-output-port))
                         #:key (worker-command (car (command-line))))
  "Serve newline-delimited MCP JSON-RPC on INPUT and OUTPUT.
Each graphical call runs in a one-shot process so Wayland or Fibers state
cannot keep the persistent protocol server from completing requests."
  ;; A worker can exit between exec and the request write.  Convert that
  ;; broken pipe into the ordinary launch-failure response instead of allowing
  ;; SIGPIPE to terminate the persistent MCP parent.
  (sigaction SIGPIPE SIG_IGN)
  (define output-lock (make-mutex))
  (define workers-lock (make-mutex))
  (define workers (make-hash-table))
  (define (send response id)
    (with-mutex output-lock
      (scm->json response output)
      (newline output)
      (force-output output)
      (trace-event "parent" "response-flushed" id (getpid))))
  (define (finish-worker! id worker)
    (with-mutex workers-lock
      (when (eq? (hash-ref workers id #f) worker)
        (hash-remove! workers id)))
    (trace-event "parent" "cleanup" id (worker-pid worker)))
  (define (read-worker-response id worker)
    (define (deliver! response)
      ;; Completion and cancellation compete for the same registry lock.
      ;; Whichever claims the request first is its sole terminal path.
      (with-mutex workers-lock
        (when (and (eq? (hash-ref workers id #f) worker)
                   (not (worker-cancelled? worker)))
          (send response id)
          (hash-remove! workers id))))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (catch #t
          (lambda ()
            (let* ((port (worker-input worker))
                   (line (read-line port))
                   (extra (read-line port))
                   (_closed (close-port port))
                   (status (cdr (waitpid (worker-pid worker))))
                   (exit-value (status:exit-val status))
                   (answer
                    (and (not (eof-object? line)) (eof-object? extra)
                         exit-value (zero? exit-value)
                         (catch #t (lambda () (json-string->scm line))
                           (lambda args #f)))))
              (trace-event "parent" "worker-exited" id (worker-pid worker)
                           (cons 'status exit-value))
              (if (and answer (valid-question-result? answer))
                  (begin
                    (trace-event "parent" "graphical-terminal" id
                                 (worker-pid worker)
                                 (cons 'status (field answer "status")))
                    (deliver! (result id (tool-result answer))))
                  (deliver! (result id (tool-error
                                        "graphical question failed"))))))
          (lambda args
            (deliver! (result id (tool-error "graphical question failed"))))))
      (lambda () (finish-worker! id worker))))
  (define (cancel-worker! id)
    (let ((worker
           (with-mutex workers-lock
             (let ((value (hash-ref workers id #f)))
               (when value
                 (set-worker-cancelled?! value #t)
                 (hash-remove! workers id))
               value))))
      (when worker
        (trace-event "parent" "cancelled" id (worker-pid worker))
        (catch 'system-error
          (lambda () (kill (worker-pid worker) SIGKILL))
          (lambda args #f))
        (join-thread (worker-thread worker)))))
  (define (expire-worker! id worker)
    ;; This watchdog runs as an ordinary Guile thread, independently of the
    ;; graphical Fibers scheduler.  It is a final lifecycle boundary for a
    ;; child whose event loop or in-dialog timer has stopped making progress.
    (let ((expired?
           (with-mutex workers-lock
             (and (eq? (hash-ref workers id #f) worker)
                  (not (worker-cancelled? worker))
                  (begin
                    (set-worker-cancelled?! worker #t)
                    (hash-remove! workers id)
                    #t)))))
      (when expired?
        (trace-event "parent" "worker-deadline" id (worker-pid worker))
        (catch 'system-error
          (lambda () (kill (worker-pid worker) SIGKILL))
          (lambda args #f))
        (join-thread (worker-thread worker))
        (send (result id (tool-result '((status . "timed-out")))) id))))
  (define (watch-worker-deadline! id worker timeout)
    ;; Polling lets this short-lived watchdog leave promptly after ordinary
    ;; completion instead of leaking a sleeping thread for the full dialog
    ;; timeout.
    (let ((deadline (+ (get-internal-real-time)
                       (* (+ timeout %worker-timeout-cleanup-margin)
                          internal-time-units-per-second))))
      (let loop ()
        (when (with-mutex workers-lock
                (eq? (hash-ref workers id #f) worker))
          (let ((remaining (/ (- deadline (get-internal-real-time))
                              internal-time-units-per-second)))
            (if (positive? remaining)
                (begin
                  (usleep (inexact->exact
                           (round (* (min remaining 0.1) 1000000))))
                  (loop))
                (expire-worker! id worker)))))))
  (define (start-tool-call! id message)
    (let* ((params (field message "params"))
           (arguments (or (field params "arguments") '())))
      (cond
       ((not (valid-timeout? arguments))
        (send (result id (tool-error
                          "question timeout must be between 0 and 240 seconds")) id))
       ((with-mutex workers-lock (hash-ref workers id #f))
        (send (error-result id -32600 "duplicate active request id") id))
       (else
        (let ((worker #f))
          (catch #t
            (lambda ()
              (set! worker (spawn-question-worker worker-command id arguments))
              (trace-event "parent" "worker-started" id (worker-pid worker))
              ;; Fast cleanup cannot overtake registration while this lock is held.
              (with-mutex workers-lock
                (let ((thread (call-with-new-thread
                               (lambda () (read-worker-response id worker)))))
                  (set-worker-thread! worker thread)
                  (hash-set! workers id worker)))
              (let ((timeout (field arguments "timeout")))
                (when timeout
                  (call-with-new-thread
                   (lambda ()
                     (watch-worker-deadline! id worker timeout))))))
            (lambda args
              ;; If thread creation or registration failed after spawning,
              ;; terminate and reap the otherwise unowned child here.
              (when (and worker (not (worker-thread worker)))
                (catch 'system-error
                  (lambda () (kill (worker-pid worker) SIGKILL))
                  (lambda args #f))
                (catch #t
                  (lambda () (close-port (worker-input worker)))
                  (lambda args #f))
                (catch 'system-error
                  (lambda () (waitpid (worker-pid worker)))
                  (lambda args #f)))
              (send (result id (tool-error "graphical question failed")) id))))))))
  (let loop ((line (read-line input)))
    (unless (eof-object? line)
      (unless (string-null? line)
        (let ((message (catch #t (lambda () (json-string->scm line))
                         (lambda args #f))))
          (if (not message)
              (send (error-result #f -32700 "invalid request") #f)
              (let ((method (field message "method"))
                    (id (field message "id")))
                (cond
                 ((and (equal? method "tools/call")
                       (equal? (field (field message "params") "name")
                               "ask_questions"))
                  (start-tool-call! id message))
                 ((equal? method "notifications/cancelled")
                  (cancel-worker! (field (field message "params") "requestId")))
                 (else
                  (let ((response (handle-mcp-message message)))
                    (when response (send response id)))))))))
      (loop (read-line input))))
  ;; EOF is teardown: no graphical child may outlive its MCP parent.
  (for-each cancel-worker!
            (with-mutex workers-lock
              (hash-map->list (lambda (id worker) id) workers))))
