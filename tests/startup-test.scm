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

(test-end "startup tracing")
