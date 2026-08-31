; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (river test-support)
             (ice-9 ftw)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64))

(define river-dir (or (getenv "GUILE_RIVER_DIR")
                      (error "GUILE_RIVER_DIR is not set")))
(define dmenu (or (getenv "GUILE_DMENU")
                  (error "GUILE_DMENU is not set")))
(define questions-command
  (or (getenv "GUILE_DMENU_QUESTIONS")
      (error "GUILE_DMENU_QUESTIONS is not set")))
(define mcp-command
  (or (getenv "GUILE_DMENU_MCP")
      (error "GUILE_DMENU_MCP is not set")))

(define (start-dmenu session)
  (let* ((input (pipe))
         (output (pipe))
         (errors (pipe))
         (pid (primitive-fork)))
    (if (zero? pid)
        (begin
          (setenv "XDG_RUNTIME_DIR" (river-session-runtime-dir session))
          (setenv "WAYLAND_DISPLAY" (river-session-display session))
          (close-port (cdr input))
          (close-port (car output))
          (close-port (car errors))
          (dup2 (fileno (car input)) 0)
          (dup2 (fileno (cdr output)) 1)
          (dup2 (fileno (cdr errors)) 2)
          (execl dmenu dmenu "--timeout" "1"))
        (begin
          (close-port (car input))
          (close-port (cdr output))
          (close-port (cdr errors))
          (display "first\nsecond\n" (cdr input))
          (close-port (cdr input))
          (list pid (car output) (car errors))))))

(define (start-questions session request)
  (let* ((input (pipe))
         (output (pipe))
         (errors (pipe))
         (pid (primitive-fork)))
    (if (zero? pid)
        (begin
          (setenv "XDG_RUNTIME_DIR" (river-session-runtime-dir session))
          (setenv "WAYLAND_DISPLAY" (river-session-display session))
          (close-port (cdr input))
          (close-port (car output))
          (close-port (car errors))
          (dup2 (fileno (car input)) 0)
          (dup2 (fileno (cdr output)) 1)
          (dup2 (fileno (cdr errors)) 2)
          (execl questions-command questions-command))
        (begin
          (close-port (car input))
          (close-port (cdr output))
          (close-port (cdr errors))
          (display request (cdr input))
          (newline (cdr input))
          (close-port (cdr input))
          (list pid (car output) (car errors))))))

(define (start-mcp session)
  (let* ((input (pipe))
         (output (pipe))
         (errors (pipe))
         (pid (primitive-fork)))
    (if (zero? pid)
        (begin
          (setenv "XDG_RUNTIME_DIR" (river-session-runtime-dir session))
          (setenv "WAYLAND_DISPLAY" (river-session-display session))
          (close-port (cdr input))
          (close-port (car output))
          (close-port (car errors))
          (dup2 (fileno (car input)) 0)
          (dup2 (fileno (cdr output)) 1)
          (dup2 (fileno (cdr errors)) 2)
          (execl mcp-command mcp-command))
        (begin
          (close-port (car input))
          (close-port (cdr output))
          (close-port (cdr errors))
          (list pid (cdr input) (car output) (car errors))))))

(define (configured-event events)
  (find (match-lambda
          (('window-configured _ 640 height) (> height 0))
          (_ #f))
        events))

(define (process-children pid)
  (let ((path (format #f "/proc/~a/task/~a/children" pid pid)))
    (if (file-exists? path)
        (string-trim-both (call-with-input-file path get-string-all))
        "")))

(define (process-thread-count pid)
  (length (filter (lambda (name) (string-every char-numeric? name))
                  (scandir (format #f "/proc/~a/task" pid)))))

(define (run-smoke session)
  (eventually "test manager readiness"
              (lambda ()
                (member '(manager-ready)
                        (read-river-manager-events session))))
  (let* ((client (start-dmenu session))
         (configured
          (eventually "dmenu acknowledged configure"
                      (lambda ()
                        (configured-event
                         (read-river-manager-events session)))))
         (status (cdr (waitpid (car client))))
         (events (read-river-manager-events session))
         (stdout (get-string-all (cadr client)))
         (stderr (get-string-all (caddr client)))
         (river-log (call-with-input-file (river-session-log session)
                      get-string-all)))
    (test-assert "manager observed dmenu app-id"
      (member '(window-app-id 1 "wl-dmenu") events))
    (test-assert "manager observed dmenu title"
      (member '(window-title 1 "dmenu") events))
    (test-assert "dmenu acknowledged positive negotiated dimensions" configured)
    (test-assert "River mapped dmenu's buffer"
      (string-contains river-log "window 'dmenu' mapped"))
    (test-equal "timeout exits without a selection" 1 (status:exit-val status))
    (test-equal "timeout leaves stdout empty" "" stdout)
    (test-equal "dmenu emitted no diagnostics" "" stderr)
    (test-assert "River reported no protocol error"
      (not (string-contains river-log "protocol error"))))
  (let* ((request
          (string-append
           "{\"timeout\":1,\"questions\":["
           "{\"id\":\"first\",\"prompt\":\"First?\",\"options\":["
           "{\"id\":\"yes\",\"label\":\"Yes\"},"
           "{\"id\":\"no\",\"label\":\"No\"}]},"
           "{\"id\":\"second\",\"prompt\":\"Second?\",\"options\":["
           "{\"id\":\"a\",\"label\":\"A\"},"
           "{\"id\":\"b\",\"label\":\"B\"}]}]}"))
         (client (start-questions session request))
         (configured
          (eventually "question command acknowledged configure"
                      (lambda ()
                        (configured-event
                         (read-river-manager-events session)))))
         (status (cdr (waitpid (car client))))
         (stdout (get-string-all (cadr client)))
         (stderr (get-string-all (caddr client)))
         (river-log (call-with-input-file (river-session-log session)
                      get-string-all)))
    (test-assert "packaged question command opened a real Wayland window"
      configured)
    (test-equal "question timeout exits successfully" 0
      (status:exit-val status))
    (test-equal "question timeout emits its structured outcome"
      "{\"status\":\"timed-out\",\"answers\":false}\n" stdout)
    (test-equal "question command emitted no diagnostics" "" stderr)
    (test-assert "question command caused no protocol error"
      (not (string-contains river-log "protocol error"))))
  (let* ((request
          (string-append
           "{\"timeout\":10,\"questions\":["
           "{\"id\":\"mode\",\"prompt\":\"Mode?\",\"options\":["
           "{\"id\":\"safe\",\"label\":\"Safe\"},"
           "{\"id\":\"fast\",\"label\":\"Fast\"}]},"
           "{\"id\":\"detail\",\"prompt\":\"Detail?\","
           "\"allowOther\":true,\"options\":["
           "{\"id\":\"short\",\"label\":\"Short\"},"
           "{\"id\":\"long\",\"label\":\"Long\"}]}]}"))
         (client (start-questions session request)))
    (eventually "interactive question window was configured"
                (lambda ()
                  (find (match-lambda
                          (('window-configured 3 _ _) #t)
                          (_ #f))
                        (read-river-manager-events session))))
    ;; Choose Fast; use Escape as Back from page two; change to Safe; then
    ;; choose Other and enter free-form text.  Pauses span the separate Wayland
    ;; sessions opened for each page and for the text reader.
    (river-session-wtype
     session
     "-s" "100" "-k" "Down" "-k" "Return"
     "-s" "400" "-k" "Escape"
     "-s" "400" "-k" "Up" "-k" "Return"
     "-s" "400" "-k" "Down" "-k" "Down" "-k" "Return"
     "-s" "400" "custom detail" "-k" "Return")
    (let ((status (cdr (waitpid (car client))))
          (stdout (get-string-all (cadr client)))
          (stderr (get-string-all (caddr client))))
      (test-equal "interactive question exits successfully" 0
        (status:exit-val status))
      (test-assert "Back permits replacing the retained first answer"
        (string-contains stdout
                         "{\"id\":\"mode\",\"answer\":\"safe\"}"))
      (test-assert "Other returns tagged free-form text"
        (string-contains
         stdout
         "{\"id\":\"detail\",\"answer\":{\"other\":\"custom detail\"}}"))
      (test-equal "interactive question emitted no diagnostics" "" stderr)))
  (let* ((request
          (string-append
           "{\"timeout\":10,\"questions\":["
           "{\"id\":\"direction\",\"prompt\":\"Direction?\",\"options\":["
           "{\"id\":\"polish\",\"label\":\"Polish\"},"
           "{\"id\":\"integrate\",\"label\":\"Integrate\"}]}]}"))
         (event-count (length (read-river-manager-events session)))
         (client (start-questions session request)))
    (eventually "comment question window was configured"
                (lambda ()
                  (configured-event
                   (drop (read-river-manager-events session) event-count))))
    ;; Ask to qualify the highlighted choice, then type in the inline editor
    ;; while the same Wayland surface keeps the selected choice visible.
    (river-session-wtype
     session
     "-s" "100" "-k" "Tab"
     "-s" "400" "cover tab comments in river" "-k" "Return")
    (let ((status (cdr (waitpid (car client))))
          (stdout (get-string-all (cadr client)))
          (stderr (get-string-all (caddr client))))
      (test-equal "choice comment exits successfully" 0
        (status:exit-val status))
      (test-assert "choice comment retains its selected stable id"
        (string-contains stdout "\"choice\":\"polish\""))
      (test-assert "choice comment retains its typed qualification"
        (string-contains stdout
                         "\"comment\":\"cover tab comments in river\""))
      (test-equal "choice comment emitted no diagnostics" "" stderr)))
  (let* ((event-count (length (read-river-manager-events session)))
         (client (start-mcp session))
         (initial-thread-count (process-thread-count (car client)))
         (request
          (string-append
           "{\"jsonrpc\":\"2.0\",\"id\":41,\"method\":\"tools/call\","
           "\"params\":{\"name\":\"ask_questions\",\"arguments\":{"
           "\"timeout\":10,\"questions\":[{\"id\":\"decision\","
           "\"prompt\":\"Proceed?\",\"options\":["
           "{\"id\":\"yes\",\"label\":\"Yes\"},"
           "{\"id\":\"no\",\"label\":\"No\"}] }]}}}")))
    (display request (cadr client))
    (newline (cadr client))
    (force-output (cadr client))
    (eventually "MCP question window was configured"
                (lambda ()
                  (configured-event
                   (drop (read-river-manager-events session) event-count))))
    (river-session-wtype session "-s" "100" "-k" "Down" "-k" "Return")
    (let ((response (read-line (caddr client))))
      (test-assert "selected MCP worker is reaped"
        (eventually "selected MCP worker cleanup"
                    (lambda ()
                      (string-null? (process-children (car client))))))
      (display
       (string-append
        "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"tools/call\","
        "\"params\":{\"name\":\"ask_questions\",\"arguments\":{"
        "\"timeout\":1,\"autoResolve\":true,\"questions\":[{"
        "\"id\":\"safe\",\"prompt\":\"Safe?\",\"options\":["
        "{\"id\":\"yes\",\"label\":\"Yes\",\"recommended\":true},"
        "{\"id\":\"no\",\"label\":\"No\"}] }]}}}\n")
       (cadr client))
      (force-output (cadr client))
      (let ((timeout-response (read-line (caddr client))))
        (test-assert "MCP auto-resolve returns its recommended stable id"
          (and (string-contains timeout-response "\"id\":42")
               (string-contains timeout-response "\"answer\":\"yes\""))))
      (let ((cancel-event-count
             (length (read-river-manager-events session))))
        (display
         (string-append
          "{\"jsonrpc\":\"2.0\",\"id\":43,\"method\":\"tools/call\","
          "\"params\":{\"name\":\"ask_questions\",\"arguments\":{"
          "\"timeout\":10,\"questions\":[{\"id\":\"cancel\","
          "\"prompt\":\"Cancel?\",\"options\":["
          "{\"id\":\"yes\",\"label\":\"Yes\"},"
          "{\"id\":\"no\",\"label\":\"No\"}] }]}}}\n")
         (cadr client))
        (force-output (cadr client))
        (eventually "cancelled MCP window was configured"
                    (lambda ()
                      (configured-event
                       (drop (read-river-manager-events session)
                             cancel-event-count))))
        (display
         "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\",\"params\":{\"requestId\":43}}\n"
         (cadr client))
        (display
         (string-append
          "{\"jsonrpc\":\"2.0\",\"id\":44,\"method\":\"tools/call\","
          "\"params\":{\"name\":\"ask_questions\",\"arguments\":{"
          "\"timeout\":1,\"questions\":[{\"id\":\"next\","
          "\"prompt\":\"Next?\",\"options\":["
          "{\"id\":\"one\",\"label\":\"One\"},"
          "{\"id\":\"two\",\"label\":\"Two\"}] }]}}}\n")
         (cadr client))
        (force-output (cadr client))
        (let ((second-response (read-line (caddr client))))
          (test-assert "a request after cancellation completes normally"
            (and (string-contains second-response "\"id\":44")
                 (not (string-contains second-response "\"id\":43"))))))
      (test-assert "MCP requests leave no child and restore thread count"
        (eventually "MCP lifecycle cleanup"
                    (lambda ()
                      (and (string-null? (process-children (car client)))
                           (= initial-thread-count
                              (process-thread-count (car client)))))))
      (close-port (cadr client))
      (let ((status (cdr (waitpid (car client))))
            (stderr (get-string-all (cadddr client))))
        (test-equal "MCP server exits successfully after stdin closes" 0
          (status:exit-val status))
        (test-assert "MCP selection returns on the JSON-RPC transport"
          (string-contains response "\"id\":41"))
        (test-assert "MCP response contains the selected stable id"
          (string-contains response "\"answer\":\"no\""))
        (test-assert "MCP response contains structured content"
          (string-contains response "\"structuredContent\""))
        (test-equal "MCP server emitted no diagnostics" "" stderr)))))

(test-begin "guile-dmenu integration")
(call-with-headless-river-session
 (lambda (session) (river-test-manager-command session river-dir))
 run-smoke
 #:root (or (getenv "TMPDIR") "/tmp"))
(test-end "guile-dmenu integration")
