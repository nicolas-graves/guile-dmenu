(define-module (guile-dmenu wayland-fibers)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers io-wakeup)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-8)  ; for receive
  #:use-module (srfi srfi-9)
  #:use-module (oop goops)

  ;; Import all the fiber modules
  #:use-module (guile-dmenu registry-fiber)
  #:use-module (guile-dmenu seat-fiber)
  #:use-module (guile-dmenu keyboard-fiber)
  #:use-module (guile-dmenu xdg-surface-fiber)
  #:use-module (guile-dmenu xdg-toplevel-fiber)

  #:export (make-wayland-fiber-connection
            wayland-fiber-connection?
            wfc-display
            wfc-registry-context
            wfc-running?

            connect-wayland-fibers
            create-window-fibers
            run-event-loop-fibers
            shutdown-wayland-fibers

            ;; Helper to get globals
            get-global))

;; Main connection object for fiber-based Wayland
(define-record-type <wayland-fiber-connection>
  (make-wayland-fiber-connection display registry-context control-channel
                                 running? dispatcher-fiber registry-processor)
  wayland-fiber-connection?
  (display wfc-display)
  (registry-context wfc-registry-context)
  (control-channel wfc-control-channel)
  (running? wfc-running? set-wfc-running?!)
  (dispatcher-fiber wfc-dispatcher-fiber set-wfc-dispatcher-fiber!)
  (registry-processor wfc-registry-processor set-wfc-registry-processor!))

;; Helper to get globals from registry
(define (get-global conn interface-key)
  (let ((registry-ctx (wfc-registry-context conn)))
    (hash-ref (registry-globals registry-ctx) interface-key)))

;; Event dispatcher fiber
(define (event-dispatcher-loop connection)
  (let ((display (wfc-display connection))
        (control-channel (wfc-control-channel connection)))

    (let loop ()
      ;; Check for control messages
      (match (perform-operation
              (choice-operation
               (wrap-operation
                (get-operation control-channel)
                (lambda (msg) msg))
               (wrap-operation
                (sleep-operation 0.01)  ; 10ms timeout
                (const 'timeout))))

        ('shutdown
         (set-wfc-running?! connection #f))

        ('timeout
         (when (wfc-running? connection)
           ;; Handle Wayland events - simplified without prepare-read
           (let ((fd (wl-display-get-fd display)))
             ;; Wait for events to be available
             (match (perform-operation
                     (choice-operation
                      (wrap-operation
                       (wait-until-port-readable-operation (fdopen fd "r"))
                       (const 'readable))
                      (wrap-operation
                       (sleep-operation 0.1)
                       (const 'timeout))))

               ('readable
                ;; Dispatch available events
                (wl-display-dispatch display))

               ('timeout
                ;; Check for pending events
                (when (not (zero? (wl-display-dispatch-pending display)))
                  #t))))
           (loop)))

        (_
         (when (wfc-running? connection)
           (loop)))))))

;; Connect to Wayland and set up all fiber-based listeners
(define (connect-wayland-fibers)
  (format #t "Connecting to Wayland display~%")
  (let ((display (wl-display-connect)))

    (unless display
      (error "Failed to connect to Wayland display"))

    (format #t "Connected to display: ~a~%" display)

    ;; Create control channel
    (let* ((control-channel (make-channel)))

      ;; Set up registry with fiber - get both context and processor thunk
      (format #t "About to setup registry fiber~%")
      (call-with-values
          (lambda () (setup-registry-fiber display))
        (lambda (registry-context processor-thunk)

          ;; Create connection object
          (let ((connection (make-wayland-fiber-connection
                             display registry-context control-channel
                             #t #f #f)))

            ;; Store the processor thunk for later
            (set-wfc-registry-processor! connection processor-thunk)

            (format #t "Connection object created~%")
            connection))))))

;; Create window with fiber-based event handling
  (define* (create-window-fibers conn title app-id width
                                 #:key
                                 (xdg-surface-configure-callback #f)
                                 (xdg-toplevel-configure-callback #f)
                                 (xdg-toplevel-close-callback #f))

  ;; Get necessary globals
  (let ((compositor (get-global conn 'compositor))
        (xdg-wm-base (get-global conn 'xdg-wm-base)))

    (unless (and compositor xdg-wm-base)
      (error "Missing required Wayland protocols"))

    ;; Create surface
    (let* ((surface (wl-compositor-create-surface compositor))
           (xdg-surface (xdg-wm-base-get-xdg-surface xdg-wm-base surface)))

      ;; Set up XDG surface fiber
      (let ((xdg-surface-context
             (setup-xdg-surface-fiber xdg-surface
                                      #:configure-callback
                                      xdg-surface-configure-callback)))

        ;; Create XDG toplevel
        (let ((xdg-toplevel (xdg-surface-get-toplevel xdg-surface)))

          ;; Set up XDG toplevel fiber
          (let ((xdg-toplevel-context
                 (setup-xdg-toplevel-fiber xdg-toplevel
                                           #:configure-callback
                                           xdg-toplevel-configure-callback
                                           #:close-callback
                                           xdg-toplevel-close-callback)))

            ;; Configure toplevel window
            (xdg-toplevel-set-title xdg-toplevel title)
            (xdg-toplevel-set-app-id xdg-toplevel app-id)

            ;; Commit surface
            (wl-surface-commit surface)

            ;; Return all the objects
            (list surface xdg-surface xdg-toplevel
                  xdg-surface-context xdg-toplevel-context)))))))

;; Run the main event loop
(define (run-event-loop-fibers conn)
  (format #t "Starting event loop fibers~%")

  ;; NOW spawn the registry processor fiber that we deferred
  (when (wfc-registry-processor conn)
    (format #t "Spawning registry processor fiber~%")
    (spawn-fiber (wfc-registry-processor conn)))

  ;; Start the event dispatcher fiber
  (let ((dispatcher (spawn-fiber
                     (lambda ()
                       (event-dispatcher-loop conn)))))
    (set-wfc-dispatcher-fiber! conn dispatcher)

    ;; Keep running while the connection is active
    (let loop ()
      (when (wfc-running? conn)
        (sleep 0.1)
        (loop)))))

;; Shutdown the connection
(define (shutdown-wayland-fibers conn)
  ;; Signal shutdown
  (put-message (wfc-control-channel conn) 'shutdown)

  ;; Wait for dispatcher to complete
  (when (wfc-dispatcher-fiber conn)
    (join-fiber (wfc-dispatcher-fiber conn)))

  ;; Disconnect from display
  (wl-display-disconnect (wfc-display conn)))

;; Convenience macro for running with fibers
(define-syntax-rule (with-wayland-fibers body ...)
  (run-fibers
   (lambda ()
     (let ((conn (connect-wayland-fibers)))
       (dynamic-wind
         (const #t)
         (lambda ()
           body ...)
         (lambda ()
           (shutdown-wayland-fibers conn)))))
   #:install-suspendable-ports? #f))
