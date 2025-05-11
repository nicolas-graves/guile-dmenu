(use-modules (guile-dmenu wayland)
             (guile-dmenu memory-utils)
             (guile-dmenu graphics)
             (guile-dmenu keyboard)
             (guile-dmenu input)
             (wayland client display)
             (wayland client protocol wayland)
             (wayland client protocol xdg-shell)
             (ice-9 match)
             (ice-9 rdelim)
             (fibers)
             (fibers channels)
             (fibers operations)
             (fibers timers)
             (srfi srfi-2)
             )

;; Prevent GC to avoid potential segment faults during drawing
(gc-disable)

;; Parameters to store important objects
(define wayland-conn (make-parameter #f))
(define width* (make-parameter 800))
(define padding* (make-parameter 4))
(define options* (make-parameter '()))
(define max-options* (make-parameter 10))
(define prompt* (make-parameter "dmenu: "))
(define selected-index* (make-parameter 0))
(define input-text* (make-parameter ""))
(define filtered-options* (make-parameter '()))
(define exit-channel* (make-parameter #f))
(define filter-channel* (make-parameter #f))
(define redraw-channel* (make-parameter #f))

;; Draw the menu using the graphics module
(define (draw-frame)
  (let ((height (* (+ 1 (min (length (filtered-options*)) (max-options*)))
                   (+ 14 (* 2 (padding*))))))
    (draw-menu (width*) height (padding*)
               (wayland-connection-shm (wayland-conn))
               (prompt*) (input-text*)
               (selected-index*) (filtered-options*) (max-options*))
    ))

;; Force a redraw of the window
(define (redraw)
  (pk 'redraw-attempt (and (wayland-conn) #t))
  (and-let* ((surface (and=> (wayland-conn) wayland-connection-surface))
             (shm (wayland-connection-shm (wayland-conn)))
             (buffer (draw-frame)))
    ;; Flush pending events before drawing
    (wl-display-flush (and=> (wayland-conn) wayland-connection-display))
    ;; (wl-display-flush display)
    (wl-surface-attach surface buffer 0 0)
    (wl-surface-damage surface 0 0 (width*) 1000)
    (wl-surface-commit surface)
    ;; Flush after committing changes
    (wl-display-flush (and=> (wayland-conn) wayland-connection-display))
    (wl-display-roundtrip (and=> (wayland-conn) wayland-connection-display))
    (pk 'redraw)
    #t))

;; Read options from stdin
(define (read-stdin)
  (let loop ((line (read-line)))
      (unless (eof-object? line)
        (options* (append (options*) (list line)))
        (loop (read-line)))))

;; Request a redraw
(define (request-redraw)
  (put-message (redraw-channel*) 'redraw)
  #t)

;; Our key handler that adapts to the keyboard module interface
(define (handle-key key)
  (let ((input-handler (make-input-handler
                        request-redraw
                        input-text*
                        selected-index*
                        filtered-options*
                        options*
                        max-options*
                        (lambda (code)
                          (put-message (exit-channel*) code)
                          #t)
                        filter-channel*)))
    (input-handler key (xkb-state))
    #t))

(define (main . args)
  (read-stdin)
  (filtered-options* (options*))
  (initialize-fallback-keymap)
  (set-key-handler! handle-key)

  (run-fibers
   (lambda ()
     (exit-channel* (make-channel))
     (filter-channel* (make-channel))
     (redraw-channel* (make-channel))
     (initialize-keyboard-fibers)

     ;; Filter processing fiber
     (spawn-fiber
      (lambda ()
        (let loop ()
          (match (get-message (filter-channel*))
            (('filter text)
             ;; Do the filtering work
             (filtered-options* (filter-options (options*) text))
             (selected-index* 0)
             ;; Request a redraw
             (request-redraw))
            (_ #t))
          (pk 'filter-loop)
          (loop))))

     (start-key-processor-fiber)

     ;; Connect to Wayland display with fiber-aware seat listener
     (let ((conn (connect-wayland wl-seat-listener-with-fibers)))
       (wayland-conn conn)

       (spawn-fiber
        (lambda ()
          (let loop ()
            (match (get-message (redraw-channel*))
              ('redraw (redraw))
              (_ #t))
            (pk 'redraw-loop)
            (loop))))

       ;; Create and configure window
       (create-window
        conn
        "dmenu"
        "wl-dmenu"
        (width*)

        ;; XDG surface configure callback
        (lambda (data xdg-surface serial)
          (xdg-surface-ack-configure xdg-surface serial)
          (redraw)
          #t)

        ;; XDG toplevel configure callback
        (lambda (data xdg width height states)
          (unless (zero? width)
            (width* width))
          #t)

        ;; XDG toplevel close callback
        (lambda (data xdg-toplevel)
          (exit 0)))

       (spawn-fiber
        (lambda ()
          (let ((display (wayland-connection-display conn)))
            (let loop ()
              (sleep 0.01)
              ;; Dispatch any pending events
              (wl-display-dispatch display)
              (wl-display-flush display)
              (pk 'wayland-loop)

              (loop)))
          #t))

       ;; Main exit handling loop
       (let loop ()
         (let ((exit-code (get-message (exit-channel*))))
           (format (current-error-port) "Exiting with code: ~a~%" exit-code)
           (primitive-exit exit-code)))))

   ;; Ensure all fibers complete before exit
   #:drain? #t))
