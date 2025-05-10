(define-module (guile-dmenu wayland-fibers)
  #:use-module (fibers channels)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)

  #:export (make-wayland-fiber-connection
            wayland-fiber-connection?
            wayland-fiber-connection-display
            wayland-fiber-connection-control-channel
            wayland-fiber-connection-event-channels
            wayland-fiber-connection-running?
            set-wayland-fiber-connection-running!

            setup-registry-listeners
            setup-seat-listeners
            setup-keyboard-listeners
            setup-xdg-surface-listeners
            setup-xdg-toplevel-listeners

            close-wayland-connection))

;; Debug function
(define (debug-log msg . args)
  (format #t "DEBUG [wayland-fibers]: ~a ~a~%" msg args)
  (force-output))

;; Connection record to hold all the state
(define-record-type <wayland-fiber-connection>
  (make-wayland-fiber-connection display control-channel event-channels running?)
  wayland-fiber-connection?
  (display wayland-fiber-connection-display)
  (control-channel wayland-fiber-connection-control-channel)
  (event-channels wayland-fiber-connection-event-channels)
  (running? wayland-fiber-connection-running? set-wayland-fiber-connection-running!))

;; Set up registry with fiber-safe listeners
(define (setup-registry-listeners connection registry)
  (debug-log "setup-registry-listeners start" connection registry)
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'registry)))
    (debug-log "Got event channel" event-channel)
    (debug-log "Creating listener object")
    (let ((listener (make <wl-registry-listener>
                      #:global
                      (lambda (data registry name interface version)
                        (debug-log "Registry global callback invoked" interface version)
                        (debug-log "About to put-message to channel")
                        ;; Safe to use put-message - we're in fiber context!
                        (put-message event-channel
                                     (list 'global registry name interface version))
                        (debug-log "put-message completed"))

                      #:global-remove
                      (lambda (data registry name)
                        (debug-log "Registry global-remove callback invoked" name)
                        (put-message event-channel
                                     (list 'global-remove registry name))))))
      (debug-log "Created listener" listener)
      (debug-log "About to call wl-registry-add-listener")
      (wl-registry-add-listener registry listener)
      (debug-log "wl-registry-add-listener completed"))))

;; Set up seat with fiber-safe listeners
(define (setup-seat-listeners connection seat)
  (debug-log "setup-seat-listeners start")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'seat)))

    (wl-seat-add-listener
      seat
      (make <wl-seat-listener>
        #:capabilities
        (lambda (data seat capabilities)
          (debug-log "Seat capabilities callback" capabilities)
          (put-message event-channel
                       (list 'capabilities seat capabilities)))

        #:name
        (lambda (data seat name)
          (debug-log "Seat name callback" name)
          (put-message event-channel
                       (list 'name seat name)))))))

;; Set up keyboard with fiber-safe listeners
(define (setup-keyboard-listeners connection keyboard)
  (debug-log "setup-keyboard-listeners start")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'keyboard)))

    (wl-keyboard-add-listener
      keyboard
      (make <wl-keyboard-listener>
        #:keymap
        (lambda (data keyboard format fd size)
          (debug-log "Keyboard keymap callback" format fd size)
          (put-message event-channel
                       (list 'keymap keyboard format fd size)))

        #:enter
        (lambda (data keyboard serial surface keys)
          (debug-log "Keyboard enter callback")
          (put-message event-channel
                       (list 'enter keyboard serial surface keys)))

        #:leave
        (lambda (data keyboard serial surface)
          (debug-log "Keyboard leave callback")
          (put-message event-channel
                       (list 'leave keyboard serial surface)))

        #:key
        (lambda (data keyboard serial time key state)
          (debug-log "Keyboard key callback" key state)
          (put-message event-channel
                       (list 'key keyboard serial time key state)))

        #:modifiers
        (lambda (data keyboard serial mods-depressed mods-latched mods-locked group)
          (debug-log "Keyboard modifiers callback")
          (put-message event-channel
                       (list 'modifiers keyboard serial mods-depressed
                             mods-latched mods-locked group)))

        #:repeat-info
        (lambda (data keyboard rate delay)
          (debug-log "Keyboard repeat-info callback" rate delay)
          (put-message event-channel
                       (list 'repeat-info keyboard rate delay)))))))

;; Set up XDG surface with fiber-safe listeners
(define (setup-xdg-surface-listeners connection xdg-surface)
  (debug-log "setup-xdg-surface-listeners start")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'xdg-surface)))

    (xdg-surface-add-listener
      xdg-surface
      (make <xdg-surface-listener>
        #:configure
        (lambda (data xdg-surface serial)
          (debug-log "XDG surface configure callback" serial)
          (put-message event-channel
                       (list 'configure xdg-surface serial)))))))

;; Set up XDG toplevel with fiber-safe listeners
(define (setup-xdg-toplevel-listeners connection xdg-toplevel)
  (debug-log "setup-xdg-toplevel-listeners start")
  (let ((event-channel (hash-ref (wayland-fiber-connection-event-channels connection) 'xdg-toplevel)))

    (xdg-toplevel-add-listener
      xdg-toplevel
      (make <xdg-toplevel-listener>
        #:configure
        (lambda (data xdg width height states)
          (debug-log "XDG toplevel configure callback" width height)
          (put-message event-channel
                       (list 'configure xdg width height states)))

        #:close
        (lambda (data xdg-toplevel)
          (debug-log "XDG toplevel close callback")
          (put-message event-channel
                       (list 'close xdg-toplevel)))))))

;; Clean shutdown
(define (close-wayland-connection connection)
  (debug-log "close-wayland-connection")
  (put-message (wayland-fiber-connection-control-channel connection) 'shutdown))
