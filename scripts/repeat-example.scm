(use-modules (fibers)
             (fibers timers)
             (fibers channels)
             (fibers operations))

(define (timed-or-stop delay stop-chan)
  (call-with-values
      (lambda ()
        (perform-operation
         (choice-operation
          (sleep-operation delay)
          (get-operation stop-chan))))
    (lambda args
      (pair? args))))

(define (repeat-loop action initial-delay repeat-delay stop-chan)
  (unless (timed-or-stop initial-delay stop-chan)
    (let loop ()
      (action)
      (unless (timed-or-stop repeat-delay stop-chan)
        (loop)))))

(define* (start-repeat action #:key (initial-delay 0.4) (repeat-delay 0.05))
  (let ((stop-chan (make-channel)))
    (spawn-fiber
     (lambda ()
       (repeat-loop action initial-delay repeat-delay stop-chan)))
    stop-chan))

(define (stop-repeat stop-chan)
  (put-message stop-chan 'stop))

;; Actual example
(run-fibers
 (lambda ()
   (define counter 0)

   (define stop
     (start-repeat
      (lambda ()
        (set! counter (+ counter 1))
        (format #t "tick ~a~%" counter))
      #:initial-delay 1.0
      #:repeat-delay 0.5))

   (spawn-fiber
    (lambda ()
      (sleep 3)
      (stop-repeat stop))))
 #:drain? #t)
