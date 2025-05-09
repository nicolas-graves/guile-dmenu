(define-module (guile-dmenu xdg-toplevel-fiber)
  #:use-module (ice-9 records)
  #:use-module (oop goops)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (wayland client protocol xdg-shell)
  #:export (make-xdg-toplevel-fiber-context
            create-fiber-xdg-toplevel-listener
            xdg-toplevel-event-processor
            setup-xdg-toplevel-fiber))

(define-record-type <xdg-toplevel-fiber-context>
  (make-xdg-toplevel-fiber-context
   configure-channel
   close-channel
   configure-callback
   close-callback)
  xdg-toplevel-fiber-context?
  (configure-channel xdg-toplevel-configure-channel)
  (close-channel xdg-toplevel-close-channel)
  (configure-callback xdg-toplevel-configure-callback set-xdg-toplevel-configure-callback!)
  (close-callback xdg-toplevel-close-callback set-xdg-toplevel-close-callback!))

;; Create a fiber-based XDG toplevel listener
(define (create-fiber-xdg-toplevel-listener xdg-toplevel-context)
  (make <xdg-toplevel-listener>
    #:configure
    (lambda (data xdg width height states)
      (put-message (xdg-toplevel-configure-channel xdg-toplevel-context)
                   (list xdg width height states)))

    #:close
    (lambda (data xdg-toplevel)
      (put-message (xdg-toplevel-close-channel xdg-toplevel-context)
                   (list xdg-toplevel)))))

;; Fiber to process XDG toplevel events
(define (xdg-toplevel-event-processor xdg-toplevel-context)
  (spawn-fiber
   (lambda ()
     (let loop ()
       (match (perform-operation
               (choice-operation
                (wrap-operation
                 (get-operation (xdg-toplevel-configure-channel xdg-toplevel-context))
                 (lambda (data) (cons 'configure data)))
                (wrap-operation
                 (get-operation (xdg-toplevel-close-channel xdg-toplevel-context))
                 (lambda (data) (cons 'close data)))))

         (('configure xdg width height states)
          ;; Call user callback if provided
          (let ((callback (xdg-toplevel-configure-callback xdg-toplevel-context)))
            (when callback
              (callback xdg width height states))))

         (('close xdg-toplevel)
          ;; Call user callback if provided
          (let ((callback (xdg-toplevel-close-callback xdg-toplevel-context)))
            (if callback
                (callback xdg-toplevel)
                ;; Default behavior: exit
                (exit 0)))))

       (loop)))))

;; Complete setup function
(define (setup-xdg-toplevel-fiber xdg-toplevel
                                   #:key
                                   (configure-callback #f)
                                   (close-callback #f))
  (let ((xdg-toplevel-context (make-xdg-toplevel-fiber-context
                               (make-channel)       ; configure
                               (make-channel)       ; close
                               configure-callback
                               close-callback)))

    ;; Add the XDG toplevel listener
    (xdg-toplevel-add-listener xdg-toplevel
                               (create-fiber-xdg-toplevel-listener xdg-toplevel-context))

    ;; Start the XDG toplevel event processor fiber
    (xdg-toplevel-event-processor xdg-toplevel-context)

    xdg-toplevel-context))
