(use-modules (guile-dmenu memory-utils)
             (guile-dmenu graphics)
             (guile-dmenu keyboard)
             (guile-dmenu input)
             (wayland client display)
             (wayland client protocol wayland)
             (wayland client protocol xdg-shell)
             (oop goops)
             (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (rnrs bytevectors))

;; Prevent GC to avoid potential segment faults during drawing
(gc-disable)

;; Parameters to store important objects
(define compositor (make-parameter #f))
(define shm (make-parameter #f))
(define xdg-wm-base (make-parameter #f))
(define xdg-toplevel (make-parameter #f))
(define wsurface (make-parameter #f))
(define seat (make-parameter #f))
(define width* (make-parameter 800))
(define padding* (make-parameter 8))
(define options* (make-parameter '()))
(define max-options* (make-parameter 10))
(define prompt* (make-parameter "dmenu: "))
(define selected-index* (make-parameter 0))
(define input-text* (make-parameter ""))
(define filtered-options* (make-parameter '()))

;; Draw the menu using the graphics module
(define (draw-frame)
  (let ((height (* (+ 1 (min (length (filtered-options*)) (max-options*)))
                   (+ 14 (* 2 (padding*))))))
    (draw-menu (width*) height (padding*) (shm) (prompt*) (input-text*)
               (selected-index*) (filtered-options*) (max-options*))))

;; Force a redraw of the window
(define (redraw)
  (when (and (wsurface) (shm))
    (let ((buffer (draw-frame)))
      (wl-surface-attach (wsurface) buffer 0 0)
      (wl-surface-commit (wsurface)))))

;; Read options from stdin
(define (read-stdin)
  (let loop ((line (read-line)))
      (unless (eof-object? line)
        (options* (append (options*) (list line)))
        (loop (read-line)))))

;; Our key handler that adapts to the keyboard module interface
(define (handle-key key)
  (let ((input-handler (make-input-handler
                        redraw
                        input-text*
                        selected-index*
                        filtered-options*
                        options*
                        max-options*)))
    (input-handler key (xkb-state))))

;; Main function
(define (main . args)
  ;; Read options from stdin
  (read-stdin)

  ;; Initialize filtered options
  (filtered-options* (options*))

  ;; Initialize XKB for keyboard handling
  (initialize-fallback-keymap)

  ;; Set our key handler
  (set-key-handler! handle-key)

  ;; Connect to Wayland display
  (let* ((w-display (wl-display-connect)))
    (unless w-display
      (format (current-error-port) "Unable to connect to wayland compositor~%")
      (exit -1))

    ;; Get registry and set up listeners
    (let ((registry (wl-display-get-registry w-display))
          (listener (make <wl-registry-listener>
                      #:global
                      (lambda (data registry name interface version)
                        (match interface
                          ("wl_compositor"
                           (compositor (wrap-wl-compositor
                                        (wl-registry-bind
                                         registry name
                                         %wl-compositor-interface 3))))
                          ("wl_shm"
                           (shm (wrap-wl-shm
                                 (wl-registry-bind registry name %wl-shm-interface 1))))
                          ("xdg_wm_base"
                           (xdg-wm-base
                            (wrap-xdg-wm-base
                             (wl-registry-bind registry name %xdg-wm-base-interface 1)))
                           (xdg-wm-base-add-listener
                            (xdg-wm-base)
                            (make <xdg-wm-base-listener>
                              #:ping (lambda (data base serial)
                                       (xdg-wm-base-pong base serial)))))
                          ("wl_seat"
                           (seat (wrap-wl-seat
                                  (wl-registry-bind registry name %wl-seat-interface 7)))
                           (wl-seat-add-listener (seat) wl-seat-listener))
                          (_
                           #t)))
                      #:global-remove
                      (lambda (data registry name)
                        #t))))

      ;; Add listener to registry
      (wl-registry-add-listener registry listener)
      (wl-display-roundtrip w-display)

      ;; Check if we have all required globals
      (unless (and (compositor) (shm) (xdg-wm-base))
        (format (current-error-port) "Missing required Wayland protocols~%")
        (exit 1))

      ;; Create surface and configure XDG surface
      (let* ((surface (wl-compositor-create-surface (compositor)))
             (xdg-surface (xdg-wm-base-get-xdg-surface (xdg-wm-base) surface)))

        ;; Set up XDG surface listener
        (xdg-surface-add-listener
         xdg-surface
         (make <xdg-surface-listener>
           #:configure
           (lambda (data xdg-surface serial)
             (xdg-surface-ack-configure xdg-surface serial)
             (redraw))))

        ;; Store surface and create XDG toplevel
        (wsurface surface)
        (xdg-toplevel (xdg-surface-get-toplevel xdg-surface))

        ;; Configure toplevel window
        (xdg-toplevel-set-title (xdg-toplevel) "dmenu")
        (xdg-toplevel-set-app-id (xdg-toplevel) "wl-dmenu")

        ;; Set up toplevel listener
        (xdg-toplevel-add-listener
         (xdg-toplevel)
         (make <xdg-toplevel-listener>
           #:configure
           (lambda (data xdg width _height states)
             (unless (zero? width)
               (width* width)))
           #:close
           (lambda (data xdg-toplevel)
             (exit 0))))

        (wl-surface-commit surface)

        ;; Main event loop
        (while (not (zero? (wl-display-dispatch w-display)))
          #t)))))
