(use-modules (guile-dmenu wayland-fibers)
             (guile-dmenu memory-utils)
             (guile-dmenu graphics)
             (guile-dmenu keyboard)
             (guile-dmenu input)
             (wayland client protocol wayland)
             (wayland client protocol xdg-shell)
             (fibers)
             (fibers channels)
             (ice-9 match)
             (ice-9 rdelim))

;; Prevent GC to avoid potential segment faults during drawing
(gc-disable)

;; Parameters to store important objects
(define wayland-conn (make-parameter #f))
(define surface* (make-parameter #f))
(define width* (make-parameter 800))
(define padding* (make-parameter 4))
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
               (get-global (wayland-conn) 'shm)
               (prompt*) (input-text*)
               (selected-index*) (filtered-options*) (max-options*))))

;; Force a redraw of the window
(define (redraw)
  (when (and (wayland-conn) (surface*))
    (let ((buffer (draw-frame)))
      (wl-surface-attach (surface*) buffer 0 0)
      (wl-surface-damage (surface*) 0 0 (width*) 1000)
      (wl-surface-commit (surface*)))))

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

;; XDG surface configure callback
(define (xdg-surface-configure-callback xdg-surface serial)
  (redraw))

;; XDG toplevel configure callback
(define (xdg-toplevel-configure-callback xdg width height states)
  (unless (zero? width)
    (width* width)))

;; XDG toplevel close callback
(define (xdg-toplevel-close-callback xdg-toplevel)
  (shutdown-wayland-fibers (wayland-conn))
  (exit 0))

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

  ;; Run with fibers - use explicit #:hz to disable preemption
  (run-fibers
   (lambda ()
     ;; Connect to Wayland display using fibers
     (let ((conn (connect-wayland-fibers)))
       (wayland-conn conn)

       ;; Create window with fiber-based event handling
       (match (create-window-fibers
               conn
               "dmenu"
               "wl-dmenu"
               (width*)
               #:xdg-surface-configure-callback xdg-surface-configure-callback
               #:xdg-toplevel-configure-callback xdg-toplevel-configure-callback
               #:xdg-toplevel-close-callback xdg-toplevel-close-callback)
         ((surface xdg-surface xdg-toplevel
                   xdg-surface-context xdg-toplevel-context)
          ;; Store the surface
          (surface* surface)

          ;; Run the event loop
          (run-event-loop-fibers conn)))

       ;; Clean shutdown
       (shutdown-wayland-fibers conn)))
   #:install-suspendable-ports? #f
   #:hz 0))  ; Disable preemption to avoid continuation barrier issues
