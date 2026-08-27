(use-modules (ice-9 match)
             (fibers)
             (fibers timers)
             (fibers channels)
             (fibers operations))

(define* (start-repeat action #:key (initial-delay 0.4) (repeat-delay 0.05))
  "Start repeating ACTION after INITIAL-DELAY, then every REPEAT-DELAY.
   Returns a stop channel - send any message to it to stop the repeat."
  (let ((stop-chan (make-channel)))
    (spawn-fiber
     (lambda ()
       (let loop ((delay initial-delay))
         (match (perform-operation
                 (choice-operation
                  (wrap-operation (sleep-operation delay)
                                  (lambda () 'continue))
                  (wrap-operation (get-operation stop-chan)
                                  (lambda (_) 'stop))))
           ('continue
            (action)
            (loop repeat-delay))
           ('stop
            #t)))))
    stop-chan))

(define (stop-repeat stop-chan)
  "Stop a repeat by sending a message to its stop channel."
  (put-message stop-chan 'stop))

;; Example usage
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
