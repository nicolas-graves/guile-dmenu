(use-modules (guile-dmenu wayland)
             (guile-dmenu memory-utils)
             (guile-dmenu graphics)
             (guile-dmenu keyboard)
             (guile-dmenu input)
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
(define wayland-conn (make-parameter #f))
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
    (draw-menu (width*) height (padding*)
               (wayland-connection-shm (wayland-conn))
               (prompt*) (input-text*)
               (selected-index*) (filtered-options*) (max-options*))))

;; Force a redraw of the window
(define (redraw)
  (when (wayland-conn)
    (let ((surface (wayland-connection-surface (wayland-conn)))
          (shm (wayland-connection-shm (wayland-conn))))
      (when (and surface shm)
        (let ((buffer (draw-frame)))
          (wl-surface-attach surface buffer 0 0)
          (wl-surface-damage surface 0 0 (width*) 1000)
          (wl-surface-commit surface))))))

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
  (let ((conn (connect-wayland wl-seat-listener)))
    (wayland-conn conn)

    ;; Create and configure window
    (create-window
     conn
     "dmenu"
     "wl-dmenu"
     (width*)

     ;; XDG surface configure callback
     (lambda (data xdg-surface serial)
       (xdg-surface-ack-configure xdg-surface serial)
       (redraw))

     ;; XDG toplevel configure callback
     (lambda (data xdg width height states)
       (unless (zero? width)
         (width* width)))

     ;; XDG toplevel close callback
     (lambda (data xdg-toplevel)
       (exit 0)))

    ;; Main event loop
    (run-event-loop conn)))
