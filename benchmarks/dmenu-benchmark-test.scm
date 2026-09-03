(define-module (benchmarks dmenu-benchmark-test)
  #:use-module (ares suitbl)
  #:use-module (guile-dmenu filter)
  #:use-module (guile-dmenu graphics)
  #:use-module (guile-dmenu swing-adapter)
  #:use-module (guile-performance-harness)
  #:use-module (guile-performance-harness suitbl)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (swing layout)
  #:export (dmenu-benchmark-tests))

(define %project-directory
  (dirname (dirname (current-filename))))

(define %guile
  (or (getenv "GUILE")
      (search-path (parse-path (getenv "PATH")) "guile")
      (error "could not find Guile for startup benchmark")))

;; Keep generated inputs deterministic.  The labels deliberately include
;; duplicates, non-ASCII text, and occasional long values.
(define (candidate index)
  (let ((base (case (modulo index 5)
                ((0) (format #f "alpha-~6,'0d" index))
                ((1) (format #f "prefix-match-~6,'0d" index))
                ((2) (format #f "~6,'0d-middle-match-tail" index))
                ((3) (format #f "café-λ-~6,'0d" index))
                (else "duplicate-entry"))))
    (if (zero? (modulo index 97))
        (string-append base "-" (make-string 256 #\x))
        base)))

(define (make-candidates size)
  (map candidate (iota size)))

(define (measure thunk)
  (let ((started (get-internal-real-time)))
    (thunk)
    (make-measured-sample
     (/ (- (get-internal-real-time) started)
        internal-time-units-per-second))))

(define %sizes '((10 100 1000 10000 100000)))

;; Each sample starts a fresh Guile process, so this captures interpreter and
;; module loading after the host's filesystem cache has warmed.  A genuinely
;; cold-cache run must be performed on a fresh machine/VM; evicting the host's
;; page cache from a benchmark would be privileged and disruptive.
(define %warm-process-startup
  (make-benchmark
   #:name "dmenu-warm-process-module-startup"
   #:warmup-count 1 #:sample-count 10 #:deadline 20
   #:operation
   (lambda (_state)
     (measure
      (lambda ()
        (let ((status
               (system* %guile "--no-auto-compile"
                        "-L" (string-append %project-directory "/src")
                        "-c" "(use-modules (guile-dmenu menu))")))
          (unless (zero? status)
            (error "dmenu startup subprocess failed" status))))))))

(define %input-ingestion
  (make-benchmark
   #:name "dmenu-input-ingestion" #:dimensions %sizes
   #:warmup-count 1 #:sample-count 5 #:deadline 20
   #:collect-guile-metrics? #t
   #:setup (lambda (size)
             (string-join (make-candidates size) "\n" 'suffix))
   #:operation
   (lambda (input _size)
     (measure
      (lambda ()
        (call-with-input-string input
          (lambda (port)
            (let loop ((line (read-line port)) (items '()))
              (if (eof-object? line)
                  (reverse items)
                  (loop (read-line port) (cons line items)))))))))))

(define %menu-construction
  (make-benchmark
   #:name "dmenu-menu-construction" #:dimensions %sizes
   #:warmup-count 1 #:sample-count 5 #:deadline 20
   #:collect-guile-metrics? #t
   #:setup make-candidates
   #:operation
   (lambda (options _size)
     (measure (lambda () (initial-state options))))))

(define %queries
  '(("" "prefix" "middle-match" "not-present" "CAFÉ")))

(define %filtering
  (make-benchmark
   #:name "dmenu-filter-per-keystroke" #:dimensions (append %sizes %queries)
   #:warmup-count 2 #:sample-count 10 #:deadline 20
   #:collect-guile-metrics? #t
   #:setup (lambda (size _query) (make-candidates size))
   #:operation
   (lambda (options _size query)
     (measure (lambda () (filter-options options query))))))

(define %query-update
  (make-benchmark
   #:name "dmenu-query-state-update" #:dimensions (append %sizes %queries)
   #:warmup-count 2 #:sample-count 10 #:deadline 10 #:total-deadline 90
   #:collect-guile-metrics? #t
   #:setup (lambda (size _query)
             (let ((options (make-candidates size)))
               (cons options (initial-state options))))
   #:operation
   (lambda (fixture _size query)
     (measure
      (lambda ()
        (fold (lambda (character state)
                (cadr (handle-input-char character state (car fixture))))
              (cdr fixture)
              (string->list query)))))))

(define %rendering
  (make-benchmark
   #:name "dmenu-layout-visible-rows" #:dimensions (append %sizes '((10 50)))
   #:warmup-count 1 #:sample-count 5 #:deadline 20
   #:collect-guile-metrics? #t
   #:setup
   (lambda (size rows)
     (initial-state (make-candidates size)))
   #:operation
   (lambda (state size rows)
     (measure
      (lambda ()
        (layout-tree
         (completion-state->swing-tree
          state #:prompt "dmenu: " #:maximum rows
          #:width 800 #:padding 4)
         #:width 800 #:height (menu-height 4 size rows)))))))

(define-benchmark-suite (dmenu-benchmark-tests)
  (benchmark-test %warm-process-startup)
  (benchmark-test %input-ingestion)
  (benchmark-test %menu-construction)
  (benchmark-test %filtering)
  (benchmark-test %query-update)
  (benchmark-test %rendering))
