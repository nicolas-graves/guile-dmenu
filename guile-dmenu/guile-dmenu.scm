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

;; Parameters to store important state
(define width* (make-parameter 800))
(define padding* (make-parameter 4))
(define options* (make-parameter '()))
(define max-options* (make-parameter 10))
(define prompt* (make-parameter "dmenu: "))
(define selected-index* (make-parameter 0))
(define input-text* (make-parameter ""))
(define filtered-options* (make-parameter '()))

;; Draw the menu using the graphics module
(define (draw-frame shm)
  (let ((height (* (+ 1 (min (length (filtered-options*)) (max-options*)))
                   (+ 14 (* 2 (padding*))))))
    (draw-menu (width*) height (padding*)
               shm (prompt*) (input-text*)
               (selected-index*) (filtered-options*) (max-options*))))

;; Perform the actual redraw
(define (do-redraw connection)
  (and-let* ((surface (and=> connection wayland-connection-surface))
             (shm (wayland-connection-shm connection))
             (buffer (draw-frame shm)))
    (wl-surface-attach surface buffer 0 0)
    (wl-surface-damage surface 0 0 (width*) 1000)
    (wl-surface-commit surface)
    #t))

;; Read options from stdin
(define (read-stdin)
  (let loop ((line (read-line)))
    (unless (eof-object? line)
      (options* (append (options*) (list line)))
      (loop (read-line)))))

(define (main . args)
  (read-stdin)
  (filtered-options* (options*))
  (initialize-fallback-keymap)

  (run-fibers
   (lambda ()
     ;; Create channels
     (let ((exit-channel (make-channel))
           (state-channel (make-channel))
           (wayland-channel (make-channel)))

       (initialize-keyboard-fibers)

       ;; Set up key decoder with direct state and exit channels
       (set-key-handler! (make-key-decoder state-channel exit-channel))
       (start-key-processor-fiber)

       ;; State processing fiber - handles all state updates
       (spawn-fiber
        (lambda ()
          (let loop ()
            (match (get-message state-channel)
              ('select
               (when (and (not (null? (filtered-options*)))
                          (< (selected-index*) (length (filtered-options*))))
                 (let ((selected (list-ref (filtered-options*) (selected-index*))))
                   (format #t "~a~%" selected)
                   (put-message exit-channel 0))))

              ('move-down
               (let* ((current (selected-index*))
                      (new-idx (if (< (+ current 1) (length (filtered-options*)))
                                   (+ current 1)
                                   current)))
                 (selected-index* new-idx)
                 (put-message wayland-channel 'redraw)))

              ('move-up
               (let* ((current (selected-index*))
                      (new-idx (if (> current 0) (- current 1) 0)))
                 (selected-index* new-idx)
                 (put-message wayland-channel 'redraw)))

              ('backspace
               (let ((current-text (input-text*)))
                 (unless (string-null? current-text)
                   (let ((new-text (string-drop-right current-text 1)))
                     (input-text* (pk 'i new-text))
                     (filtered-options* (filter-options (options*) new-text))
                     (selected-index* 0)
                     (put-message wayland-channel 'redraw)))))

              (('input-char char)
               (let* ((current-text (input-text*))
                      (new-text (string-append current-text (string char))))
                 (input-text* (pk 'i new-text))
                 (filtered-options* (filter-options (options*) new-text))
                 (selected-index* 0)
                 (put-message wayland-channel 'redraw)))

              (_ #t))
            (loop))))

       ;; Connect to Wayland display
       (let ((conn (connect-wayland wl-seat-listener-with-fibers)))

         ;; Wayland event loop fiber
         (spawn-fiber
          (lambda ()
            (let ((display (wayland-connection-display conn)))
              (let loop ()
                (let ((op (choice-operation
                           (get-operation wayland-channel)
                           (wrap-operation (sleep-operation 0.001)
                                           (lambda () 'timeout)))))
                  (match (perform-operation op)
                    ('redraw
                     (do-redraw conn)
                     (wl-display-flush display))
                    ('timeout
                     (wl-display-dispatch display))
                    (_ #t)))
                (loop))
              #t)))

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

         ;; Exit handling loop
         (let loop ()
           (let ((exit-code (get-message exit-channel)))
             (format (current-error-port) "Exiting with code: ~a~%" exit-code)
             (primitive-exit exit-code))))))

   #:drain? #t))
