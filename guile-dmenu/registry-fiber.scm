(define-module (guile-dmenu registry-fiber)
  #:use-module (ice-9 records)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (guile-dmenu seat-fiber)
  #:export (make-registry-fiber-context
            create-fiber-registry-listener
            registry-event-processor
            setup-registry-fiber))

(define-record-type <registry-fiber-context>
  (make-registry-fiber-context
   global-channel
   global-remove-channel
   globals)  ; Hash table to store global objects
  registry-fiber-context?
  (global-channel registry-global-channel)
  (global-remove-channel registry-global-remove-channel)
  (globals registry-globals))

;; Create a fiber-based registry listener
(define (create-fiber-registry-listener registry-context)
  (make <wl-registry-listener>
    #:global
    (lambda (data registry name interface version)
      (put-message (registry-global-channel registry-context)
                   (list registry name interface version)))

    #:global-remove
    (lambda (data registry name)
      (put-message (registry-global-remove-channel registry-context)
                   (list registry name)))))

;; Handle global announcement
(define (handle-registry-global registry-context registry name interface version)
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
       ;; Set up seat fiber
       (let ((seat-context (setup-seat-fiber seat)))
         (hash-set! (registry-globals registry-context) 'seat-context seat-context))))

    (_ #t)))

;; Fiber to process registry events
(define (registry-event-processor registry-context)
  (spawn-fiber
   (lambda ()
     (let loop ()
       (match (perform-operation
               (choice-operation
                (wrap-operation
                 (get-operation (registry-global-channel registry-context))
                 (lambda (data) (cons 'global data)))
                (wrap-operation
                 (get-operation (registry-global-remove-channel registry-context))
                 (lambda (data) (cons 'global-remove data)))))

         (('global registry name interface version)
          (handle-registry-global registry-context registry name interface version))

         (('global-remove registry name)
          ;; Handle global removal if needed
          #t))

       (loop)))))

;; Complete setup function
(define (setup-registry-fiber display)
  (let ((registry (wl-display-get-registry display))
        (registry-context (make-registry-fiber-context
                           (make-channel)     ; global
                           (make-channel)     ; global-remove
                           (make-hash-table))))  ; globals

    ;; Add the registry listener
    (wl-registry-add-listener registry (create-fiber-registry-listener registry-context))

    ;; Start the registry event processor fiber
    (registry-event-processor registry-context)

    ;; Do initial roundtrip to get globals
    (wl-display-roundtrip display)

    registry-context))
