(use-modules (guile-dmenu wayland-fibers)
             (guile-dmenu memory-utils)
             (guile-dmenu graphics)
             (guile-dmenu keyboard)
             (guile-dmenu input)
             (wayland client protocol wayland)
             (wayland client protocol xdg-shell)
             (wayland client display)
             (fibers)
             (fibers channels)
             (fibers operations)
             (ice-9 match)
             (ice-9 rdelim))

;; Prevent GC to avoid potential segment faults during drawing
(gc-disable)

;; Debug function
(define (debug-log msg . args)
  (format #t "DEBUG: ~a ~a~%" msg args)
  (force-output))

;; Global state
(define *connection* #f)
(define *compositor* #f)
(define *shm* #f)
(define *xdg-wm-base* #f)
(define *seat* #f)
(define *keyboard* #f)
(define *surface* #f)
(define *xdg-surface* #f)
(define *xdg-toplevel* #f)

;; Parameters for UI state
(define width* (make-parameter 800))
(define padding* (make-parameter 4))
(define options* (make-parameter '()))
(define max-options* (make-parameter 10))
(define prompt* (make-parameter "dmenu: "))
(define selected-index* (make-parameter 0))
(define input-text* (make-parameter ""))
(define filtered-options* (make-parameter '()))

;; Draw the menu
(define (draw-frame)
  (when (and *surface* *shm*)
    (let ((height (* (+ 1 (min (length (filtered-options*)) (max-options*)))
                     (+ 14 (* 2 (padding*))))))
      (draw-menu (width*) height (padding*)
                 *shm*
                 (prompt*) (input-text*)
                 (selected-index*) (filtered-options*) (max-options*)))))

;; Force a redraw of the window
(define (redraw)
  (when *surface*
    (let ((buffer (draw-frame)))
      (wl-surface-attach *surface* buffer 0 0)
      (wl-surface-damage *surface* 0 0 (width*) 1000)
      (wl-surface-commit *surface*))))

;; Read options from stdin
(define (read-stdin)
  (debug-log "Starting read-stdin")
  (let loop ((line (read-line)))
    (unless (eof-object? line)
      (debug-log "Read line" line)
      (options* (append (options*) (list line)))
      (loop (read-line))))
  (debug-log "Finished read-stdin" (options*)))

;; Handle keyboard input
(define (handle-key key)
  (let ((input-handler (make-input-handler
                        redraw
                        input-text*
                        selected-index*
                        filtered-options*
                        options*
                        max-options*)))
    (input-handler key (xkb-state))))

;; Process registry events
(define (registry-event-processor connection)
  (debug-log "Starting registry-event-processor")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'registry)))
    (debug-log "Got registry event channel" event-channel)
    (spawn-fiber
      (lambda ()
        (debug-log "Registry processor fiber started")
        (let loop ()
          (match (get-message event-channel)
            (('global registry name interface version)
             (format #t "Global: ~a v~a~%" interface version)
             (match interface
               ("wl_compositor"
                (set! *compositor* (wrap-wl-compositor
                                     (wl-registry-bind registry name %wl-compositor-interface 3))))

               ("wl_shm"
                (set! *shm* (wrap-wl-shm
                              (wl-registry-bind registry name %wl-shm-interface 1))))

               ("xdg_wm_base"
                (set! *xdg-wm-base* (wrap-xdg-wm-base
                                      (wl-registry-bind registry name %xdg-wm-base-interface 1)))
                (xdg-wm-base-add-listener
                  *xdg-wm-base*
                  (make <xdg-wm-base-listener>
                    #:ping (lambda (data base serial)
                             (xdg-wm-base-pong base serial)))))

               ("wl_seat"
                (set! *seat* (wrap-wl-seat
                               (wl-registry-bind registry name %wl-seat-interface 7)))
                (setup-seat-listeners connection *seat*))

               (_ #t)))

            (('global-remove registry name)
             (format #t "Global removed: ~a~%" name)))

          (loop))))))

;; Process seat events
(define (seat-event-processor connection)
  (debug-log "Starting seat-event-processor")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'seat)))
    (spawn-fiber
      (lambda ()
        (let loop ()
          (match (get-message event-channel)
            (('capabilities seat capabilities)
             ;; Check if keyboard capability is available (bit 1)
             (when (logand capabilities 2)
               (set! *keyboard* (wl-seat-get-keyboard seat))
               (setup-keyboard-listeners connection *keyboard*)))

            (('name seat name)
             (format #t "Seat name: ~a~%" name)))

          (loop))))))

;; Process keyboard events
(define (keyboard-event-processor connection)
  (debug-log "Starting keyboard-event-processor")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'keyboard)))
    (spawn-fiber
      (lambda ()
        (let loop ()
          (match (get-message event-channel)
            (('keymap keyboard format fd size)
             (process-keymap format fd size)
             (close-fdes fd))

            (('enter keyboard serial surface keys)
             #t)

            (('leave keyboard serial surface)
             #t)

            (('key keyboard serial time key state)
             (handle-key-internal key state))

            (('modifiers keyboard serial mods-depressed mods-latched mods-locked group)
             (when (xkb-state)
               (xkb-state-update-mask (xkb-state)
                                      mods-depressed
                                      mods-latched
                                      mods-locked
                                      0 0 group)))

            (('repeat-info keyboard rate delay)
             #t))

          (loop))))))

;; Process XDG surface events
(define (xdg-surface-event-processor connection)
  (debug-log "Starting xdg-surface-event-processor")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'xdg-surface)))
    (spawn-fiber
      (lambda ()
        (let loop ()
          (match (get-message event-channel)
            (('configure xdg-surface serial)
             (xdg-surface-ack-configure xdg-surface serial)
             (redraw)))

          (loop))))))

;; Process XDG toplevel events
(define (xdg-toplevel-event-processor connection)
  (debug-log "Starting xdg-toplevel-event-processor")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'xdg-toplevel)))
    (spawn-fiber
      (lambda ()
        (let loop ()
          (match (get-message event-channel)
            (('configure xdg width height states)
             (unless (zero? width)
               (width* width)))

            (('close xdg-toplevel)
             (format #t "Window closed by compositor~%")
             (close-wayland-connection connection)
             (exit 0)))

          (loop))))))

;; Create the window
(define (create-window)
  (let* ((surface (wl-compositor-create-surface *compositor*))
         (xdg-surface (xdg-wm-base-get-xdg-surface *xdg-wm-base* surface)))

    (set! *surface* surface)
    (set! *xdg-surface* xdg-surface)

    ;; Set up XDG surface listeners
    (setup-xdg-surface-listeners *connection* xdg-surface)

    ;; Create toplevel window
    (let ((xdg-toplevel (xdg-surface-get-toplevel xdg-surface)))
      (set! *xdg-toplevel* xdg-toplevel)

      ;; Set up XDG toplevel listeners
      (setup-xdg-toplevel-listeners *connection* xdg-toplevel)

      ;; Configure the window
      (xdg-toplevel-set-title xdg-toplevel "dmenu")
      (xdg-toplevel-set-app-id xdg-toplevel "wl-dmenu")

      ;; Commit the surface
      (wl-surface-commit surface))))

;; Main function
(define (main . args)
  (debug-log "Starting main")

  ;; Read options from stdin
  (read-stdin)

  ;; Initialize filtered options
  (filtered-options* (options*))

  (debug-log "Initializing XKB")
  ;; Initialize XKB for keyboard handling
  (initialize-fallback-keymap)

  ;; Set our key handler
  (set-key-handler! handle-key)

  (debug-log "Starting run-fibers")
  ;; Run with fibers
  (run-fibers
    (lambda ()
      (debug-log "Inside run-fibers lambda")

      (debug-log "Calling wl-display-connect")
      ;; Connect to Wayland
      (let ((display (wl-display-connect)))
        (debug-log "wl-display-connect returned" display)
        (unless display
          (error "Failed to connect to Wayland display"))

        (debug-log "Creating channels")
        (let* ((control-channel (make-channel))
               (event-channels (make-hash-table)))

          (debug-log "Initializing event channels")
          ;; Initialize event channels
          (hash-set! event-channels 'registry (make-channel))
          (hash-set! event-channels 'seat (make-channel))
          (hash-set! event-channels 'keyboard (make-channel))
          (hash-set! event-channels 'surface (make-channel))
          (hash-set! event-channels 'xdg-surface (make-channel))
          (hash-set! event-channels 'xdg-toplevel (make-channel))

          (debug-log "Creating connection")
          (let ((connection (make-wayland-fiber-connection
                              display control-channel event-channels #t)))
            (set! *connection* connection)

            (debug-log "About to spawn event reader fiber")
            ;; Start the event reader fiber
            (spawn-fiber
              (lambda ()
                (format #t "Starting Wayland event reader fiber~%")
                (let loop ()
                  (when (wayland-fiber-connection-running? connection)
                    (loop)
                    ;; Use select to wait for events on the file descriptor
                    (let ((fd (wl-display-get-fd display)))
                      (select (list fd) '() '() 0 100000)  ; 100ms timeout
                      (wl-display-dispatch display))

                    (loop)))))

            (debug-log "Getting registry")
            ;; Get the registry and set up listeners
            (let ((registry (wl-display-get-registry display)))
              (debug-log "Got registry" registry)
              (debug-log "Setting up registry listeners")
              (setup-registry-listeners connection registry))

            (debug-log "Starting event processors")
            ;; Start all event processors
            (registry-event-processor connection)
            (seat-event-processor connection)
            (keyboard-event-processor connection)
            (xdg-surface-event-processor connection)
            (xdg-toplevel-event-processor connection)

            (debug-log "Calling wl-display-roundtrip")
            ;; Do initial roundtrip to get globals
            (wl-display-roundtrip display)

            (debug-log "Sleeping 0.1")
            ;; Wait a bit for globals to be set up
            (sleep 0.1)

            ;; Check if we have required globals
            (unless (and *compositor* *shm* *xdg-wm-base*)
              (format (current-error-port) "Missing required Wayland protocols~%")
              (exit 1))

            ;; Create the window
            (create-window)

            ;; Keep running until shutdown
            (let loop ()
              (sleep 1)
              (when (wayland-fiber-connection-running? connection)
                (loop)))

            ;; Clean up
            (format #t "Cleaning up~%")
            (wl-display-disconnect display)))))

    #:install-suspendable-ports? #f
    #:hz 0))  ; Disable preemption to avoid any remaining issues
