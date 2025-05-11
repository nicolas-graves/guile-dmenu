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
             (srfi srfi-2))

;; Prevent GC to avoid potential segment faults during drawing
(gc-disable)

;; Parameters to store important objects
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
(define wayland-channel* (make-parameter #f))  ;; New channel for Wayland commands

;; Draw the menu using the graphics module
(define (draw-frame shm)
  (let ((height (* (+ 1 (min (length (filtered-options*)) (max-options*)))
                   (+ 14 (* 2 (padding*))))))
    (draw-menu (width*) height (padding*)
               shm (prompt*) (input-text*)
               (selected-index*) (filtered-options*) (max-options*))))

;; Request a redraw through the Wayland command channel
(define (request-redraw)
  (put-message (wayland-channel*) 'redraw)
  #t)

;; Perform the actual redraw (called only from Wayland event loop fiber)
(define (do-redraw connection)
  (and-let* ((surface (and=> connection wayland-connection-surface))
             (shm (wayland-connection-shm connection))
             (buffer (draw-frame shm)))
    (pk 'buffer-found)
    (wl-surface-attach surface buffer 0 0)
    (wl-surface-damage surface 0 0 (width*) 1000)
    (wl-surface-commit surface)
    (pk 'redraw)
    #t))

;; Read options from stdin
(define (read-stdin)
  (let loop ((line (read-line)))
    (unless (eof-object? line)
      (options* (append (options*) (list line)))
      (loop (read-line)))))

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

  (run-fibers
   (lambda ()
     (exit-channel* (make-channel))
     (filter-channel* (make-channel))
     (wayland-channel* (make-channel))
     (initialize-keyboard-fibers)
     (set-key-handler! handle-key)
     (start-key-processor-fiber)

     ;; Connect to Wayland display with fiber-aware seat listener
     (let ((conn (connect-wayland wl-seat-listener-with-fibers)))
       (spawn-fiber
        (lambda ()
          (let ((display (wayland-connection-display conn)))
            (let loop ()
              ;; Check for Wayland commands with a short timeout
              (let ((op (choice-operation
                         (get-operation (wayland-channel*))
                         (wrap-operation (sleep-operation 0.001)
                                         (lambda () 'timeout)))))
                (match (perform-operation op)
                  ('redraw
                   (do-redraw conn)
                   (wl-display-flush display))
                  ('timeout  ;; timeout occurred
                   ;; Process Wayland events
                   (wl-display-dispatch display))
                  (other
                   (pk 'other other)
                   ;; Handle unexpected values
                   #t)))
              (loop)))
          #t))

       (spawn-fiber
        (lambda ()
          (let loop ()
            (match (get-message (filter-channel*))
              (('filter text)
               ;; Do the filtering work
               (filtered-options* (pk 'f (filter-options (options*) text)))
               (selected-index* 0)
               ;; (request-redraw)
               (do-redraw conn)
               #t)
              (_ #t))
            (loop))
          #t))

       ;; Create and configure window
       (create-window
        conn
        "dmenu"
        "wl-dmenu"
        (width*)

        ;; XDG surface configure callback
        (lambda (data xdg-surface serial)
          (xdg-surface-ack-configure xdg-surface serial)
          (do-redraw conn)
          #t)

        ;; XDG toplevel configure callback
        (lambda (data xdg width height states)
          (unless (zero? width)
            (width* width))
          #t)

        ;; XDG toplevel close callback
        (lambda (data xdg-toplevel)
          (exit 0)))

       ;; Single Wayland event loop fiber that handles everything


       ;; Main exit handling loop
       (let loop ()
         (let ((exit-code (get-message (exit-channel*))))
           (format (current-error-port) "Exiting with code: ~a~%" exit-code)
           (primitive-exit exit-code)))))

   ;; Ensure all fibers complete before exit
   #:drain? #t))
