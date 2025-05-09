(define-module (guile-dmenu wayland-fibers)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers conditions)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (ice-9 match)
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
                                 running? dispatcher-fiber)
  wayland-fiber-connection?
  (display wfc-display)
  (registry-context wfc-registry-context)
  (control-channel wfc-control-channel)
  (running? wfc-running? set-wfc-running?!)
  (dispatcher-fiber wfc-dispatcher-fiber set-wfc-dispatcher-fiber!))

;; Helper to get globals from registry
(define (get-global conn interface-key)
  (hash-ref (registry-globals (wfc-registry-context conn)) interface-key))

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
           ;; Handle Wayland events
           (let ((fd (wl-display-get-fd display)))
             ;; Prepare to read events
             (when (zero? (wl-display-prepare-read display))
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
                  (wl-display-read-events display))

                 ('timeout
                  (wl-display-cancel-read display)))))

           ;; Dispatch pending events
           (wl-display-dispatch-pending display)
           (loop)))

        (_
         (when (wfc-running? connection)
           (loop))))))

;; Connect to Wayland and set up all fiber-based listeners
(define (connect-wayland-fibers)
  (let ((display (wl-display-connect)))

    (unless display
      (error "Failed to connect to Wayland display"))

    ;; Create control channel
    (let* ((control-channel (make-channel))
           ;; Set up registry with fiber
           (registry-context (setup-registry-fiber display))
           ;; Create connection object
           (connection (make-wayland-fiber-connection
                        display registry-context control-channel
                        #t #f)))

      connection)))

;; Create window with fiber-based event handling
(define (create-window-fibers conn title app-id width
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
