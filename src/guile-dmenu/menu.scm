(define-module (guile-dmenu menu)
  #:use-module (guile-dmenu wayland)
  #:use-module (guile-dmenu graphics)
  #:use-module (guile-dmenu keyboard)
  #:use-module (guile-dmenu filter)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (ice-9 match)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers timers)
  #:export (completing-read menu-width menu-padding menu-max-options))

(define menu-width (make-parameter 800))
(define menu-padding (make-parameter 4))
(define menu-max-options (make-parameter #f))

(define (draw-state prompt message-lines input-enabled? conn cache state maximum)
  (let ((surface (wayland-connection-surface conn)))
    (when surface
      (let ((buffer (draw-menu (menu-width) 0 (menu-padding) cache prompt
                               (completing-read-state-input-text state)
                               (completing-read-state-selected-index state)
                               (completing-read-state-filtered-options state)
                               maximum #:message-lines message-lines
                               #:input-enabled? input-enabled?)))
        (when buffer
          (wl-surface-attach surface buffer 0 0)
          (wl-surface-damage surface 0 0 (menu-width) 10000)
          (wl-surface-commit surface))))))

(define* (completing-read prompt collection
                          #:key (message #f) (input-enabled? #t)
                          (timeout #f) (max-message-lines 12))
  "Display COLLECTION and return the selected string, or #f on cancellation."
  (let* ((maximum (or (menu-max-options) (length collection)))
         (message-lines (if message
                            (wrap-message-lines message
                                                (max 20 (quotient (menu-width) 8))
                                                max-message-lines)
                            '())))
    (run-fibers
     (lambda ()
       (let ((session (make-keyboard-session))
             (events (make-channel))
             (result (make-channel)))
         (set-keyboard-session-key-handler!
          session (make-key-decoder session events result))
         (let* ((conn (connect-wayland (make-seat-listener session)))
                (display (wayland-connection-display conn))
                (port (fdes->inport (wl-display-get-fd display)))
                (state (initial-state collection))
                (read-prepared? #f)
                (cache (make-menu-buffer-cache
                        (wayland-connection-shm conn)
                        #:request-redraw
                        (lambda () (spawn-fiber (lambda ()
                                                  (put-message events 'redraw)))))))
           (define (redraw s)
             (draw-state prompt message-lines input-enabled? conn cache s maximum)
             (wl-display-flush display))
           (create-window
            conn "dmenu" "wl-dmenu" (menu-width)
            (lambda (data surface serial)
              (xdg-surface-ack-configure surface serial)
              (spawn-fiber (lambda () (put-message events 'redraw))) #t)
            (lambda (data toplevel width height states)
              (unless (zero? width) (menu-width width)) #t)
            (lambda args (spawn-fiber (lambda () (put-message result #f))) #t))
           ;; Send the initial empty surface commit before waiting for the
           ;; compositor's configure event.  Redrawing before configure is a
           ;; protocol error, but waiting without flushing deadlocks startup.
           (wl-display-flush display)
           (spawn-fiber
            (lambda ()
              (let loop ((s state))
                ;; Drain callbacks already queued by libwayland, then prepare
                ;; exactly one nonblocking socket read.  If another fiber's
                ;; event wins the race below, cancel that prepared read.
                (let prepare ()
                  (when (negative? (wl-display-prepare-read display))
                    (when (negative? (wl-display-dispatch-pending display))
                      (put-message result #f))
                    (prepare)))
                (set! read-prepared? #t)
                (wl-display-flush display)
                (let ((event (perform-operation
                              (choice-operation
                               (get-operation events)
                               (wrap-operation (wait-until-port-readable-operation port)
                                               (lambda () 'wayland))))))
                  (unless (eq? event 'wayland)
                    (wl-display-cancel-read display)
                    (set! read-prepared? #f))
                  (match event
                    ('select
                     (match (handle-select s)
                       (('selected value) (put-message result value))
                       (_ (loop s))))
                    ('next (match (handle-next s)
                             (('state-update n) (redraw n) (loop n))
                             (_ (loop s))))
                    ('previous (match (handle-previous s)
                                 (('state-update n) (redraw n) (loop n))
                                 (_ (loop s))))
                    ((and (or 'backspace 'ctrl+backspace) key)
                     (if input-enabled?
                         (match (handle-backspace s collection
                                                  #:ctrl-pressed? (eq? key 'ctrl+backspace))
                           (('state-update n) (redraw n) (loop n))
                           (_ (loop s)))
                         (loop s)))
                    (('input-char char)
                     (if input-enabled?
                         (match (handle-input-char char s collection)
                           (('state-update n) (redraw n) (loop n))
                           (_ (loop s)))
                         (loop s)))
                    ((or 'redraw 'surface-configure) (redraw s) (loop s))
                    ('wayland (set! read-prepared? #f)
                              (if (or (negative? (wl-display-read-events display))
                                      (negative? (wl-display-dispatch-pending display)))
                                  (put-message result #f)
                                  (loop s)))
                    (_ (loop s)))))))
           (let ((answer
                  (perform-operation
                   (if timeout
                       (choice-operation
                        (get-operation result)
                        (wrap-operation (sleep-operation timeout) (lambda () #f)))
                       (get-operation result)))))
             (when read-prepared?
               (wl-display-cancel-read display)
               (set! read-prepared? #f))
             (wl-display-disconnect display)
             answer))))
     #:parallelism 1
     #:drain? #f)))
