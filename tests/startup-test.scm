(use-modules (guile-dmenu startup)
             (srfi srfi-64))

(test-begin "startup tracing")

(let ((output
       (call-with-output-string
         (lambda (port)
           (parameterize ((startup-trace-enabled? #t)
                          (startup-trace-port port))
             (report-startup-phase! 'test-phase))))))
  (test-assert "enabled tracing reports its phase"
    (string-contains output "phase=test-phase"))
  (test-assert "trace includes relative time"
    (string-contains output "elapsed_seconds="))
  (test-assert "trace includes an externally comparable clock"
    (string-contains output "monotonic_seconds=")))

(let ((output
       (dynamic-wind
         (lambda ()
           (setenv "GUILE_DMENU_MCP_TRACE" "stderr")
           (setenv "GUILE_DMENU_MCP_REQUEST_ID" "42"))
         (lambda ()
           (with-error-to-string
             (lambda () (report-runtime-event! 'keyboard-ready))))
         (lambda ()
           (unsetenv "GUILE_DMENU_MCP_TRACE")
           (unsetenv "GUILE_DMENU_MCP_REQUEST_ID")))))
  (test-assert "runtime tracing records a payload-free child milestone"
    (and (string-contains output "\"event\":\"keyboard-ready\"")
         (string-contains output "\"requestId\":42")
         (not (string-contains output "prompt")))))

(test-end "startup tracing")
