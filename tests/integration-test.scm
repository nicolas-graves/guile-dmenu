; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (river test-support)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-64))

(define river-dir (or (getenv "GUILE_RIVER_DIR")
                      (error "GUILE_RIVER_DIR is not set")))
(define dmenu (or (getenv "GUILE_DMENU")
                  (error "GUILE_DMENU is not set")))
(define questions-command
  (or (getenv "GUILE_DMENU_QUESTIONS")
      (error "GUILE_DMENU_QUESTIONS is not set")))

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

(define (start-questions session)
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
          (display
           (string-append
            "{\"timeout\":1,\"questions\":["
            "{\"id\":\"first\",\"prompt\":\"First?\",\"options\":["
            "{\"id\":\"yes\",\"label\":\"Yes\"},"
            "{\"id\":\"no\",\"label\":\"No\"}]},"
            "{\"id\":\"second\",\"prompt\":\"Second?\",\"options\":["
            "{\"id\":\"a\",\"label\":\"A\"},"
            "{\"id\":\"b\",\"label\":\"B\"}]}]}\n")
           (cdr input))
          (close-port (cdr input))
          (list pid (car output) (car errors))))))

(define (configured-event events)
  (find (match-lambda
          (('window-configured _ 640 height) (> height 0))
          (_ #f))
        events))

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
  (let* ((client (start-questions session))
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
    (test-assert "question timeout emits its structured outcome"
      (string-contains stdout "\"status\":\"timed-out\""))
    (test-equal "question command emitted no diagnostics" "" stderr)
    (test-assert "question command caused no protocol error"
      (not (string-contains river-log "protocol error")))))

(test-begin "guile-dmenu integration")
(call-with-headless-river-session
 (lambda (session) (river-test-manager-command session river-dir))
 run-smoke
 #:root (or (getenv "TMPDIR") "/tmp"))
(test-end "guile-dmenu integration")
