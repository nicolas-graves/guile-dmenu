(define-module (guile-dmenu wayland)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (wayland-connection
            make-wayland-connection
            wayland-connection?
            wayland-connection-display
            wayland-connection-compositor
            wayland-connection-shm
            wayland-connection-xdg-wm-base
            wayland-connection-seat
            wayland-connection-surface
            wayland-connection-xdg-surface
            wayland-connection-xdg-toplevel
            connect-wayland
            create-window
            run-event-loop))

;; Define a record type to hold all Wayland-related objects
(define-record-type <wayland-connection>
  (make-wayland-connection display compositor shm xdg-wm-base seat
                          surface xdg-surface xdg-toplevel
                          registry-listener xdg-wm-base-listener seat-listener
                          xdg-surface-listener xdg-toplevel-listener)
  wayland-connection?
  (display wayland-connection-display set-wayland-connection-display!)
  (compositor wayland-connection-compositor set-wayland-connection-compositor!)
  (shm wayland-connection-shm set-wayland-connection-shm!)
  (xdg-wm-base wayland-connection-xdg-wm-base set-wayland-connection-xdg-wm-base!)
  (seat wayland-connection-seat set-wayland-connection-seat!)
  (surface wayland-connection-surface set-wayland-connection-surface!)
  (xdg-surface wayland-connection-xdg-surface set-wayland-connection-xdg-surface!)
  (xdg-toplevel wayland-connection-xdg-toplevel set-wayland-connection-xdg-toplevel!)
  ;; Kept alive here so their FFI callback trampolines aren't GC'd out from
  ;; under libwayland, which holds only raw addresses into these listeners.
  (registry-listener wayland-connection-registry-listener set-wayland-connection-registry-listener!)
  (xdg-wm-base-listener wayland-connection-xdg-wm-base-listener set-wayland-connection-xdg-wm-base-listener!)
  (seat-listener wayland-connection-seat-listener set-wayland-connection-seat-listener!)
  (xdg-surface-listener wayland-connection-xdg-surface-listener set-wayland-connection-xdg-surface-listener!)
  (xdg-toplevel-listener wayland-connection-xdg-toplevel-listener set-wayland-connection-xdg-toplevel-listener!))

;; Connect to Wayland and set up all needed global objects
(define (connect-wayland seat-listener)
  (let ((conn (make-wayland-connection #f #f #f #f #f #f #f #f #f #f #f #f #f))
        (w-display (wl-display-connect)))

    (unless w-display
      (format (current-error-port) "Unable to connect to wayland compositor~%")
      (exit -1))

    (set-wayland-connection-display! conn w-display)
    (set-wayland-connection-seat-listener! conn seat-listener)

    ;; Get registry and set up listeners
    (let ((registry (wl-display-get-registry w-display))
          (listener (make <wl-registry-listener>
                      #:global
                      (lambda (data registry name interface version)
                        (match interface
                          ("wl_compositor"
                           (set-wayland-connection-compositor!
                            conn
                            (wrap-wl-compositor
                             (wl-registry-bind
                              registry name
                              %wl-compositor-interface 3))))

                          ("wl_shm"
                           (set-wayland-connection-shm!
                            conn
                            (wrap-wl-shm
                             (wl-registry-bind registry name %wl-shm-interface 1))))

                          ("xdg_wm_base"
                           (let ((xdg-wm-base
                                  (wrap-xdg-wm-base
                                   (wl-registry-bind registry name %xdg-wm-base-interface 1)))
                                 (xdg-wm-base-listener
                                  (make <xdg-wm-base-listener>
                                    #:ping (lambda (data base serial)
                                             (xdg-wm-base-pong base serial)))))
                             (set-wayland-connection-xdg-wm-base! conn xdg-wm-base)
                             (set-wayland-connection-xdg-wm-base-listener! conn xdg-wm-base-listener)
                             (xdg-wm-base-add-listener xdg-wm-base xdg-wm-base-listener)))

                          ("wl_seat"
                           (let ((seat (wrap-wl-seat
                                       (wl-registry-bind registry name %wl-seat-interface 7))))
                             (set-wayland-connection-seat! conn seat)
                             (wl-seat-add-listener seat seat-listener)))

                          (_ #t)))

                      #:global-remove
                      (lambda (data registry name)
                        #t))))

      ;; Add listener to registry
      (set-wayland-connection-registry-listener! conn listener)
      (wl-registry-add-listener registry listener)
      (wl-display-roundtrip w-display)

      ;; Check if we have all required globals
      (unless (and (wayland-connection-compositor conn)
                   (wayland-connection-shm conn)
                   (wayland-connection-xdg-wm-base conn))
        (format (current-error-port) "Missing required Wayland protocols~%")
        (exit 1))

      conn)))

;; Create window with surface and XDG toplevel
(define (create-window conn title app-id width xdg-surface-configure-callback
                      xdg-toplevel-configure-callback xdg-toplevel-close-callback)
  (let* ((surface (wl-compositor-create-surface
                   (wayland-connection-compositor conn)))
         (xdg-surface (xdg-wm-base-get-xdg-surface
                       (wayland-connection-xdg-wm-base conn)
                       surface)))

    ;; Set up XDG surface listener
    (let ((xdg-surface-listener
           (make <xdg-surface-listener>
             #:configure xdg-surface-configure-callback)))
      (set-wayland-connection-xdg-surface-listener! conn xdg-surface-listener)
      (xdg-surface-add-listener xdg-surface xdg-surface-listener))

    ;; Store surface and create XDG toplevel
    (set-wayland-connection-surface! conn surface)
    (set-wayland-connection-xdg-surface! conn xdg-surface)

    (let ((xdg-toplevel (xdg-surface-get-toplevel xdg-surface)))
      (set-wayland-connection-xdg-toplevel! conn xdg-toplevel)

      ;; Configure toplevel window
      (xdg-toplevel-set-title xdg-toplevel title)
      (xdg-toplevel-set-app-id xdg-toplevel app-id)

      ;; Set up toplevel listener
      (let ((xdg-toplevel-listener
             (make <xdg-toplevel-listener>
               #:configure xdg-toplevel-configure-callback
               #:close xdg-toplevel-close-callback)))
        (set-wayland-connection-xdg-toplevel-listener! conn xdg-toplevel-listener)
        (xdg-toplevel-add-listener xdg-toplevel xdg-toplevel-listener))

      (wl-surface-commit surface)

      conn)))

;; Run the main event loop
(define (run-event-loop conn)
  (while (not (zero? (wl-display-dispatch (wayland-connection-display conn))))
    (usleep 1)))
