(define-module (guile-dmenu seat-fiber)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (wayland client protocol wayland)
  #:use-module (guile-dmenu keyboard-fiber)
  #:export (make-seat-fiber-context
            create-fiber-seat-listener
            seat-event-processor
            setup-seat-fiber))

(define-record-type <seat-fiber-context>
  (make-seat-fiber-context
   capabilities-channel
   name-channel
   keyboard-channel
   pointer-channel
   touch-channel)
  seat-fiber-context?
  (capabilities-channel seat-capabilities-channel)
  (name-channel seat-name-channel)
  (keyboard-channel seat-keyboard-channel)
  (pointer-channel seat-pointer-channel)
  (touch-channel seat-touch-channel))

;; Create a fiber-based seat listener
(define (create-fiber-seat-listener seat-context)
  (make <wl-seat-listener>
    #:capabilities
    (lambda (data seat capabilities)
      ;; Put the event on the capabilities channel
      (put-message (seat-capabilities-channel seat-context)
                   (list seat capabilities)))
    #:name
    (lambda (data seat name)
      ;; Put the event on the name channel
      (put-message (seat-name-channel seat-context)
                   (list seat name)))))

;; Initialize keyboard when capabilities indicate it's available
(define (handle-seat-capabilities seat-context seat capabilities)
  ;; Check if keyboard capability is available (bit 1)
  (when (logand capabilities 2)
    (let ((keyboard (wl-seat-get-keyboard seat)))
      ;; Store keyboard reference
      (put-message (seat-keyboard-channel seat-context) keyboard)
      ;; Set up keyboard fiber
      (let ((kbd-context (setup-keyboard-fiber keyboard)))
        kbd-context))))

;; Fiber to process seat events
(define (seat-event-processor seat-context)
  (spawn-fiber
   (lambda ()
     (let loop ()
       ;; Use choice-operation to wait on multiple channels
       (match (perform-operation
               (choice-operation
                (wrap-operation
                 (get-operation (seat-capabilities-channel seat-context))
                 (lambda (data) (cons 'capabilities data)))
                (wrap-operation
                 (get-operation (seat-name-channel seat-context))
                 (lambda (data) (cons 'name data)))))

         (('capabilities seat capabilities)
          (handle-seat-capabilities seat-context seat capabilities))

         (('name seat name)
          ;; Handle seat name event
          (format #t "Seat name: ~a~%" name)))

       (loop)))))

;; Complete setup function
(define (setup-seat-fiber seat)
  (let ((seat-context (make-seat-fiber-context
                       (make-channel)  ; capabilities
                       (make-channel)  ; name
                       (make-channel)  ; keyboard
                       (make-channel)  ; pointer
                       (make-channel))))  ; touch

    (format #t "Seat context created~%")

    ;; Add the seat listener
    (wl-seat-add-listener seat (create-fiber-seat-listener seat-context))

    (format #t "About to add seat listener~%")

    (spawn-fiber (seat-event-processor seat-context))
    (sleep 0.01)

    (format #t "Seat listener added~%")

    ;; Start the seat event processor fiber

    seat-context))
