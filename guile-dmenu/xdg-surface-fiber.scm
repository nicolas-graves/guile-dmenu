(define-module (guile-dmenu xdg-surface-fiber)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (wayland client protocol xdg-shell)
  #:export (make-xdg-surface-fiber-context
            create-fiber-xdg-surface-listener
            xdg-surface-event-processor
            setup-xdg-surface-fiber))

(define-record-type <xdg-surface-fiber-context>
  (make-xdg-surface-fiber-context
   configure-channel
   configure-callback)
  xdg-surface-fiber-context?
  (configure-channel xdg-surface-configure-channel)
  (configure-callback xdg-surface-configure-callback set-xdg-surface-configure-callback!))

;; Create a fiber-based XDG surface listener
(define (create-fiber-xdg-surface-listener xdg-surface-context)
  (make <xdg-surface-listener>
    #:configure
    (lambda (data xdg-surface serial)
      (put-message (xdg-surface-configure-channel xdg-surface-context)
                   (list xdg-surface serial)))))

;; Fiber to process XDG surface events
(define (xdg-surface-event-processor xdg-surface-context)
  (spawn-fiber
   (lambda ()
     (let loop ()
       (match (get-message (xdg-surface-configure-channel xdg-surface-context))
         ((xdg-surface serial)
          ;; Acknowledge configuration
          (xdg-surface-ack-configure xdg-surface serial)
          ;; Call user callback if provided
          (let ((callback (xdg-surface-configure-callback xdg-surface-context)))
            (when callback
              (callback xdg-surface serial)))))

       (loop)))))

;; Complete setup function
(define* (setup-xdg-surface-fiber xdg-surface #:key (configure-callback #f))
  (let ((xdg-surface-context (make-xdg-surface-fiber-context
                              (make-channel)      ; configure
                              configure-callback)))

    (format #t "XDG Surface context created~%")

    ;; Add the XDG surface listener
    (xdg-surface-add-listener
     xdg-surface
     (create-fiber-xdg-surface-listener xdg-surface-context))

    (format #t "XDG Surface listener added~%")

    ;; Start the XDG surface event processor fiber
    (spawn-fiber (xdg-surface-event-processor xdg-surface-context))
    (sleep 0.01)

    (format #t "XDG Surface processor fiber added~%")

    xdg-surface-context))
