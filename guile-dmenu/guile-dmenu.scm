(use-modules (guile-dmenu wayland)
             (guile-dmenu memory-utils)
             (guile-dmenu graphics)
             (guile-dmenu keyboard)
             (guile-dmenu filter)
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

;; Single parameter to store dmenu state
(define state* (make-parameter #f))
(define width* (make-parameter 800))
(define padding* (make-parameter 4))
(define prompt* (make-parameter "dmenu: "))

;; Draw the menu using the graphics module
(define (draw-frame shm max-options)
  (let* ((state (state*))
         (height (* (+ 1 (min (length (completing-read-state-filtered-options state)) max-options))
                    (+ 14 (* 2 (padding*))))))
    (draw-menu (width*) height (padding*)
               shm (prompt*) (completing-read-state-input-text state)
               (completing-read-state-selected-index state)
               (completing-read-state-filtered-options state) max-options)))

;; Perform the actual redraw
(define (do-redraw connection max-options)
  (and-let* ((surface (and=> connection wayland-connection-surface))
             (shm (wayland-connection-shm connection))
             (buffer (draw-frame shm max-options)))
    (wl-surface-attach surface buffer 0 0)
    (wl-surface-damage surface 0 0 (width*) 1000)
    (wl-surface-commit surface)
    #t))

(define (read-stdin)
  (let loop ((line (read-line))
             (options '()))
    (if (eof-object? line)
        (reverse options)
        (loop (read-line) (cons line options)))))

(define (main . args)
  (let* ((options (read-stdin))
         (max-options (length options)))

    (state* (initial-state options))
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
                 (match (handle-select (state*))
                   (('exit code)
                    (put-message exit-channel code))
                   (('no-change _) #t)
                   (_ #t)))

                ('move-down
                 (match (handle-move-down (state*))
                   (('state-update new-state)
                    (state* new-state)
                    (put-message wayland-channel 'redraw))
                   (_ #t)))

                ('move-up
                 (match (handle-move-up (state*))
                   (('state-update new-state)
                    (state* new-state)
                    (put-message wayland-channel 'redraw))
                   (_ #t)))

                ('backspace
                 (match (handle-backspace (state*) options)
                   (('state-update new-state)
                    (state* new-state)
                    (pk 'input (completing-read-state-input-text (state*)))
                    (put-message wayland-channel 'redraw))
                   (('no-change _) #t)
                   (_ #t)))

                (('input-char char)
                 (match (handle-input-char char (state*) options)
                   (('state-update new-state)
                    (state* new-state)
                    (pk 'input (completing-read-state-input-text (state*)))
                    (put-message wayland-channel 'redraw))
                   (_ #t)))

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
                       (do-redraw conn max-options)
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
              (do-redraw conn max-options)
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

     #:drain? #t)))
