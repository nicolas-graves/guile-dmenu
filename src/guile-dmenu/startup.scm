(define-module (guile-dmenu startup)
  #:use-module (ice-9 format)
  #:export (startup-trace-enabled?
            startup-trace-port
            report-startup-phase!
            report-runtime-event!))

(define (monotonic-seconds)
  (/ (get-internal-real-time) internal-time-units-per-second))

;; Loading this small module before the rest of dmenu gives phase timestamps a
;; stable in-process origin.  External harnesses can use MONOTONIC_SECONDS to
;; relate it to a timestamp taken immediately before spawning Guile.
(define %startup-origin (monotonic-seconds))
(define %reported-phases '())

(define startup-trace-enabled?
  (make-parameter
   (let ((value (getenv "GUILE_DMENU_STARTUP_TRACE")))
     (and value (not (string-null? value)) (not (string=? value "0"))))))

(define startup-trace-port (make-parameter (current-error-port)))

(define (report-startup-phase! phase)
  "Write an opt-in, machine-readable timestamp for PHASE to standard error."
  (when (and (startup-trace-enabled?) (not (memq phase %reported-phases)))
    (let ((now (monotonic-seconds))
          (port (startup-trace-port)))
      (set! %reported-phases (cons phase %reported-phases))
      (format port
              "guile-dmenu-startup phase=~a elapsed_seconds=~,6f monotonic_seconds=~,6f~%"
              phase (- now %startup-origin) now)
      (force-output port))))

(define (report-runtime-event! event)
  "Append payload-free EVENT diagnostics to the MCP trace, when enabled."
  (let ((destination (getenv "GUILE_DMENU_MCP_TRACE"))
        (request-id (getenv "GUILE_DMENU_MCP_REQUEST_ID")))
    (when (and destination request-id
               (not (string-null? destination))
               (not (string-null? request-id)))
      (catch #t
        (lambda ()
          (let ((port (if (string=? destination "stderr")
                          (current-error-port)
                          (open-file destination "a"))))
            (format port
                    "{\"time\":~a,\"role\":\"child\",\"event\":\"~a\",\"requestId\":~a,\"pid\":~a}~%"
                    (get-internal-real-time) event request-id (getpid))
            (force-output port)
            (unless (string=? destination "stderr") (close-port port))))
        (lambda args #f)))))
