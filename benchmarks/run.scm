(use-modules (ares suitbl)
             ((ares suitbl reporters) #:prefix reporter:)
             ((ares suitbl state) #:prefix state:)
             (benchmarks dmenu-benchmark-test)
             (guile-performance-harness suitbl))

(current-test-runner
 (make-suitbl
  #:config `((auto-run? . #f)
             (schedule-tests . ,benchmarks-only)
             (test-reporter . ,(reporter:reporter-every
                                (list reporter:compact benchmark-reporter))))))
(dmenu-benchmark-tests)
((current-test-runner) '((type . runner/run-tests)))
(let ((summary (state:get-run-summary (get-state))))
  (exit (if (and summary
                 (zero? (assoc-ref summary 'failures))
                 (zero? (assoc-ref summary 'errors)))
            0 1)))
