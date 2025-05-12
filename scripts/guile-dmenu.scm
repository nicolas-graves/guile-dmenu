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

;; Configuration parameters - these don't change during execution
(define width* (make-parameter 800))
(define padding* (make-parameter 4))
(define prompt* (make-parameter "dmenu: "))

(define (draw connection state max-options)
  (and-let* ((surface (and=> connection wayland-connection-surface))
             (input-text (completing-read-state-input-text state))
             (options (completing-read-state-filtered-options state))
             (selected-index (completing-read-state-selected-index state))
             (shm (wayland-connection-shm connection))
             (height (* (+ 1 (min (length options) max-options))
                        (+ 14 (* 2 (padding*)))))
             (buffer (draw-menu (width*)
                                height
                                (padding*)
                                shm
                                (prompt*)
                                input-text
                                selected-index
                                options
                                max-options)))
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

    (initialize-fallback-keymap)

    (run-fibers
     (lambda ()
       ;; Create a unified channel for all application events
       (let ((app-channel (make-channel))
             (exit-channel (make-channel)))

         (initialize-keyboard-fibers)

         ;; Create initial state
         (let ((initial-program-state (initial-state options)))

           ;; Set up key decoder with the app channel and exit channel
           (set-key-handler! (make-key-decoder app-channel exit-channel))
           (start-key-processor-fiber)

           ;; Connect to Wayland display
           (let ((conn (connect-wayland wl-seat-listener-with-fibers)))

             ;; Main application loop - processes all events and maintains state
             (spawn-fiber
              (lambda ()
                (let ((display (wayland-connection-display conn)))
                  (let loop ((current-state initial-program-state))
                    ;; Poll for events with timeout
                    (let ((op (choice-operation
                               (get-operation app-channel)
                               (wrap-operation (sleep-operation 0.001)
                                               (lambda () 'timeout)))))

                      (let ((next-state
                             (match (perform-operation op)
                               ;; ---- State Operations ----
                               ('select
                                (match (handle-select current-state)
                                  (('exit code)
                                   (put-message exit-channel code)
                                   current-state)
                                  (('no-change _)
                                   current-state)
                                  (_ current-state)))

                               ('next
                                (match (handle-next current-state)
                                  (('state-update new-state)
                                   (draw conn new-state max-options)
                                   (wl-display-flush display)
                                   new-state)
                                  (_ current-state)))

                               ('previous
                                (match (handle-previous current-state)
                                  (('state-update new-state)
                                   (draw conn new-state max-options)
                                   (wl-display-flush display)
                                   new-state)
                                  (_ current-state)))

                               ('backspace
                                (match (handle-backspace current-state options)
                                  (('state-update new-state)
                                   (pk 'input (completing-read-state-input-text new-state))
                                   (draw conn new-state max-options)
                                   (wl-display-flush display)
                                   new-state)
                                  (('no-change _)
                                   current-state)
                                  (_ current-state)))

                               (('input-char char)
                                (match (handle-input-char char current-state options)
                                  (('state-update new-state)
                                   (pk 'input (completing-read-state-input-text new-state))
                                   (draw conn new-state max-options)
                                   (wl-display-flush display)
                                   new-state)
                                  (_ current-state)))

                               ;; ---- Wayland Operations ----
                               ('redraw
                                (draw conn current-state max-options)
                                (wl-display-flush display)
                                current-state)

                               ('surface-configure
                                (draw conn current-state max-options)
                                (wl-display-flush display)
                                current-state)

                               ('timeout
                                (wl-display-dispatch display)
                                current-state)

                               (_ current-state))))

                        (loop next-state))))
                  #t)))

             ;; Initial draw request
             (put-message app-channel 'redraw)

             ;; Create and configure window
             (create-window
              conn
              "dmenu"
              "wl-dmenu"
              (width*)

              ;; XDG surface configure callback
              (lambda (data xdg-surface serial)
                (xdg-surface-ack-configure xdg-surface serial)
                ;; Request a redraw after surface configuration
                (spawn-fiber
                 (lambda ()
                   (put-message app-channel 'surface-configure)))
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
                 (primitive-exit exit-code)))))))

     #:drain? #t)))
