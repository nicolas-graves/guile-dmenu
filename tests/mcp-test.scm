(use-modules (guile-dmenu mcp)
             (ice-9 ftw)
             (ice-9 textual-ports)
             (ice-9 threads)
             (json)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64))

(test-begin "dmenu MCP server")

(define (field object name)
  (cdr (or (assoc name object) (assoc (string->symbol name) object))))

(define fixture-command
  (list "guile" "--no-auto-compile"
        (canonicalize-path
         (string-append (dirname (car (command-line)))
                        "/mcp-worker-fixture.scm"))))

(define (eventually predicate)
  (let loop ((remaining 500))
    (cond ((predicate) #t)
          ((zero? remaining) #f)
          (else (usleep 10000) (loop (- remaining 1))))))

(define (start-test-server output)
  (let ((input (pipe)))
    (list (cdr input)
          (call-with-new-thread
           (lambda ()
             (run-mcp-server (car input) output
                             #:worker-command fixture-command))))))

(define (stop-test-server server)
  (close-port (car server))
  (join-thread (cadr server)))

(define (call-fixture id context)
  (let* ((output (open-output-string))
         (server (start-test-server output)))
    (format (car server)
            "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"tools/call\",\"params\":{\"name\":\"ask_questions\",\"arguments\":{\"context\":~s}}}~%"
            id context)
    (force-output (car server))
    (eventually (lambda ()
                  (positive? (string-count (get-output-string output)
                                           #\newline))))
    (stop-test-server server)
    (json-string->scm (string-trim-right (get-output-string output)))))

(let* ((response (handle-mcp-message
                  '((jsonrpc . "2.0") (id . 1) (method . "initialize")
                    (params . ()))))
       (payload (field response "result")))
  (test-equal "initialize reports the MCP protocol"
    "2025-06-18" (field payload "protocolVersion")))

(let* ((response (handle-mcp-message
                  '((jsonrpc . "2.0") (id . 2) (method . "tools/list")
                    (params . ()))))
       (tools (field (field response "result") "tools")))
  (test-equal "question tool is advertised"
    "ask_questions" (field (vector-ref tools 0) "name")))

(parameterize
    ((mcp-question-handler
      (lambda (arguments)
        '((status . "answered")
          (answers . #(((id . "choice") (answer . "yes"))))))))
  (let* ((message
          '((jsonrpc . "2.0")
            (id . 3)
            (method . "tools/call")
            (params
             . ((name . "ask_questions")
                (arguments
                 . ((questions
                     . #(((id . "choice")
                          (prompt . "Continue?")
                          (options
                           . #(((id . "yes") (label . "Yes"))
                               ((id . "no") (label . "No")))))))))))))
         (response (handle-mcp-message message))
         (payload (field response "result"))
         (structured (field payload "structuredContent")))
    (test-equal "tool call returns structured answers"
      "answered" (field structured "status"))
    (test-assert "successful tool call is not an error"
      (not (field payload "isError")))))

(let* ((response (call-fixture 4 "timed-out"))
       (payload (field (field response "result") "structuredContent")))
  (test-equal "worker timeout is returned as a terminal result"
    "timed-out" (field payload "status")))

(let* ((output (open-output-string))
       (server (start-test-server output)))
  (display
   "{\"jsonrpc\":\"2.0\",\"id\":40,\"method\":\"tools/call\",\"params\":{\"name\":\"ask_questions\",\"arguments\":{\"context\":\"sleep\",\"timeout\":0.05}}}\n"
   (car server))
  (force-output (car server))
  (test-assert "parent watchdog terminates a stuck graphical worker"
    (eventually (lambda ()
                  (string-contains (get-output-string output)
                                   "\"status\":\"timed-out\""))))
  (stop-test-server server))

(let ((trace (format #f "/tmp/guile-dmenu-mcp-trace-~a" (getpid))))
  (when (file-exists? trace) (delete-file trace))
  (parameterize ((mcp-trace-destination trace))
    (call-fixture 9 "secret-context-do-not-log"))
  (let ((text (call-with-input-file trace get-string-all)))
    (test-assert "trace covers worker start, flush, and cleanup"
      (and (string-contains text "\"event\":\"worker-started\"")
           (string-contains text "\"event\":\"response-flushed\"")
           (string-contains text "\"event\":\"cleanup\"")))
    (test-assert "trace contains no request payload"
      (not (string-contains text "secret-context-do-not-log"))))
  (delete-file trace))

(for-each
 (lambda (entry)
   (let* ((response (call-fixture (car entry) (cdr entry)))
          (payload (field response "result")))
     (test-assert (string-append (cdr entry) " worker output is rejected")
       (field payload "isError"))))
 '((5 . "failure") (6 . "malformed") (7 . "extra")))

(parameterize ((mcp-question-handler (lambda (arguments) (error "failure"))))
  (let ((output (open-output-string)))
    (test-equal "question worker failure uses a nonzero exit status"
      1 (call-with-input-string "{}"
          (lambda (input) (run-question-worker input output))))
    (test-equal "question worker failure emits one generic JSON line"
      "{\"error\":\"graphical question failed\"}\n"
      (get-output-string output))))

(let* ((response (handle-mcp-message
                  '((jsonrpc . "2.0") (id . 8) (method . "tools/call")
                    (params . ((name . "ask_questions")
                               (arguments . ((timeout . 241))))))))
       (payload (field response "result")))
  (test-assert "timeouts above 240 seconds are rejected" (field payload "isError")))

(let* ((output (open-output-string))
       (server (start-test-server output))
       (input (car server)))
    (display
     (string-append
      "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/call\",\"params\":{\"name\":\"ask_questions\",\"arguments\":{\"context\":\"sleep\"}}}\n"
      "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"ping\",\"params\":{}}\n"
      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\",\"params\":{\"requestId\":10}}\n") input)
    (force-output input)
    (test-assert "ping arrives before teardown"
      (eventually (lambda ()
                    (string-contains (get-output-string output) "\"id\":11"))))
    (stop-test-server server)
    (let ((text (get-output-string output)))
      (test-assert "a pending question does not block protocol traffic"
        (string-contains text "\"id\":11"))
      (test-assert "cancelled questions emit no stale response"
        (not (string-contains text "\"id\":10")))))

;; Exercise the historical race where a fast worker removed its entry before
;; the main thread registered it, leaving stale worker-table state behind.
(let* ((count 100)
         (input
          (string-concatenate
           (map (lambda (id)
                  (format #f
                          "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"tools/call\",\"params\":{\"name\":\"ask_questions\",\"arguments\":{}}}~%"
                          id))
                (iota count))))
         (output (open-output-string))
         (server (start-test-server output)))
    (display input (car server))
    (force-output (car server))
    (test-assert "all fast workers complete before teardown"
      (eventually
       (lambda ()
         (= count (string-count (get-output-string output) #\newline)))))
    (stop-test-server server)
    (let ((responses (string-count (get-output-string output) #\newline)))
      (test-equal "fast worker registration is race-free" count responses)))

(test-end "dmenu MCP server")
