(define-module (guile-dmenu registry-fiber)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (guile-dmenu seat-fiber)
  #:export (make-registry-fiber-context
            create-fiber-registry-listener
            registry-event-processor
            setup-registry-fiber
            registry-globals))

(define-record-type <registry-fiber-context>
  (make-registry-fiber-context
   global-channel
   global-remove-channel
   spawn-channel
   globals)  ; Hash table to store global objects
  registry-fiber-context?
  (global-channel registry-global-channel)
  (global-remove-channel registry-global-remove-channel)
  (spawn-channel registry-spawn-channel)  ; Channel for deferred fiber spawning
  (globals registry-globals))

;; Create a fiber-based registry listener
(define (create-fiber-registry-listener registry-context)
  (format #t "Creating registry listener~%")
  (make <wl-registry-listener>
    #:global
    (lambda (data registry name interface version)
      (format #t "Registry global callback: ~a ~a v~a~%" interface name version)
      (put-message (registry-global-channel registry-context)
                   (list registry name interface version)))

    #:global-remove
    (lambda (data registry name)
      (format #t "Registry global-remove callback: ~a~%" name)
      (put-message (registry-global-remove-channel registry-context)
                   (list registry name)))))

;; Handle global announcement
(define (handle-registry-global registry-context registry name interface version)
  (format #t "Handling registry global: ~a~%" interface)
  ;; Store the global in the hash table
  (hash-set! (registry-globals registry-context) interface
             (list name version registry))

  ;; Handle specific interfaces
  (match interface
    ("wl_compositor"
     (let ((compositor (wrap-wl-compositor
                        (wl-registry-bind registry name %wl-compositor-interface 3))))
       (hash-set! (registry-globals registry-context) 'compositor compositor)))

    ("wl_shm"
     (let ((shm (wrap-wl-shm
                 (wl-registry-bind registry name %wl-shm-interface 1))))
       (hash-set! (registry-globals registry-context) 'shm shm)))

    ("xdg_wm_base"
     (let ((xdg-wm-base (wrap-xdg-wm-base
                         (wl-registry-bind registry name %xdg-wm-base-interface 1))))
       (hash-set! (registry-globals registry-context) 'xdg-wm-base xdg-wm-base)
       ;; Set up XDG WM base ping handler
       (xdg-wm-base-add-listener
        xdg-wm-base
        (make <xdg-wm-base-listener>
          #:ping (lambda (data base serial)
                   (xdg-wm-base-pong base serial))))))

    ("wl_seat"
     (let ((seat (wrap-wl-seat
                  (wl-registry-bind registry name %wl-seat-interface 7))))
       (hash-set! (registry-globals registry-context) 'seat seat)
       ;; Queue seat setup to be done in fiber context
       (format #t "Queueing seat setup~%")
       (put-message (registry-spawn-channel registry-context)
                    (list 'setup-seat seat))))

    (_ #t)))

;; Fiber to process registry events - returns a thunk to be spawned later
(define (registry-event-processor registry-context)
  (format #t "Creating registry event processor thunk~%")
  (lambda ()
    (format #t "Registry event processor fiber started~%")
    (let loop ()
      (format #t "Waiting for registry events...~%")
      (match (perform-operation
              (choice-operation
               (wrap-operation
                (get-operation (registry-global-channel registry-context))
                (lambda (data) (cons 'global data)))
               (wrap-operation
                (get-operation (registry-global-remove-channel registry-context))
                (lambda (data) (cons 'global-remove data)))
               (wrap-operation
                (get-operation (registry-spawn-channel registry-context))
                (lambda (data) (cons 'spawn data)))))

        (('global registry name interface version)
         (format #t "Processing global event: ~a~%" interface)
         (handle-registry-global registry-context registry name interface version))

        (('global-remove registry name)
         (format #t "Processing global-remove event: ~a~%" name)
         #t)

        (('spawn 'setup-seat seat)
         (format #t "Processing spawn event: setup-seat~%")
         ;; Now we can safely spawn the seat fiber
         (let ((seat-context (setup-seat-fiber seat)))
           (hash-set! (registry-globals registry-context) 'seat-context seat-context))))

      (loop))))

;; Complete setup function - returns context and processor thunk
(define (setup-registry-fiber display)
  (format #t "Setting up registry fiber~%")
  (let ((registry (wl-display-get-registry display))
        (registry-context (make-registry-fiber-context
                           (make-channel)     ; global
                           (make-channel)     ; global-remove
                           (make-channel)     ; spawn channel
                           (make-hash-table))))  ; globals

    (format #t "Registry context created~%")

    ;; Add the registry listener
    (wl-registry-add-listener registry (create-fiber-registry-listener registry-context))

    (format #t "Registry listener added~%")

    ;; Create the processor thunk but don't spawn it yet
    (let ((processor-thunk (registry-event-processor registry-context)))

      (format #t "Doing initial roundtrip~%")

      ;; Do initial roundtrip to get globals
      (wl-display-roundtrip display)

      (format #t "Roundtrip complete~%")

      ;; Return both the context and the processor thunk using values
      (values registry-context processor-thunk))))
