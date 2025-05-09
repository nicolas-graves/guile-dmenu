(define-module (guile-dmenu keyboard-fiber)
  #:use-module (ice-9 records)
  #:use-module (oop goops)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (wayland client protocol wayland)
  #:use-module (guile-dmenu keyboard)  ; For process-keymap, xkb functions
  #:export (make-keyboard-fiber-context
            create-fiber-keyboard-listener
            keyboard-event-processor
            setup-keyboard-fiber))

(define-record-type <keyboard-fiber-context>
  (make-keyboard-fiber-context
   keymap-channel
   enter-channel
   leave-channel
   key-channel
   modifiers-channel
   repeat-info-channel)
  keyboard-fiber-context?
  (keymap-channel keyboard-keymap-channel)
  (enter-channel keyboard-enter-channel)
  (leave-channel keyboard-leave-channel)
  (key-channel keyboard-key-channel)
  (modifiers-channel keyboard-modifiers-channel)
  (repeat-info-channel keyboard-repeat-info-channel))

;; Create a fiber-based keyboard listener
(define (create-fiber-keyboard-listener kbd-context)
  (make <wl-keyboard-listener>
    #:keymap
    (lambda (data keyboard format fd size)
      (put-message (keyboard-keymap-channel kbd-context)
                   (list keyboard format fd size)))

    #:enter
    (lambda (data keyboard serial surface keys)
      (put-message (keyboard-enter-channel kbd-context)
                   (list keyboard serial surface keys)))

    #:leave
    (lambda (data keyboard serial surface)
      (put-message (keyboard-leave-channel kbd-context)
                   (list keyboard serial surface)))

    #:key
    (lambda (data keyboard serial time key state)
      (put-message (keyboard-key-channel kbd-context)
                   (list keyboard serial time key state)))

    #:modifiers
    (lambda (data keyboard serial mods-depressed mods-latched mods-locked group)
      (put-message (keyboard-modifiers-channel kbd-context)
                   (list keyboard serial mods-depressed mods-latched mods-locked group)))

    #:repeat-info
    (lambda (data keyboard rate delay)
      (put-message (keyboard-repeat-info-channel kbd-context)
                   (list keyboard rate delay)))))

;; Fiber to process keyboard events
(define (keyboard-event-processor kbd-context)
  (spawn-fiber
   (lambda ()
     (let loop ()
       ;; Wait for keyboard events using choice-operation
       (match (perform-operation
               (choice-operation
                (wrap-operation
                 (get-operation (keyboard-keymap-channel kbd-context))
                 (lambda (data) (cons 'keymap data)))
                (wrap-operation
                 (get-operation (keyboard-enter-channel kbd-context))
                 (lambda (data) (cons 'enter data)))
                (wrap-operation
                 (get-operation (keyboard-leave-channel kbd-context))
                 (lambda (data) (cons 'leave data)))
                (wrap-operation
                 (get-operation (keyboard-key-channel kbd-context))
                 (lambda (data) (cons 'key data)))
                (wrap-operation
                 (get-operation (keyboard-modifiers-channel kbd-context))
                 (lambda (data) (cons 'modifiers data)))
                (wrap-operation
                 (get-operation (keyboard-repeat-info-channel kbd-context))
                 (lambda (data) (cons 'repeat-info data)))))

         (('keymap keyboard format fd size)
          ;; Process keymap using existing keyboard module function
          (process-keymap format fd size)
          (close-fdes fd))

         (('enter keyboard serial surface keys)
          ;; Handle keyboard enter
          #t)

         (('leave keyboard serial surface)
          ;; Handle keyboard leave
          #t)

         (('key keyboard serial time key state)
          ;; Handle key event using existing keyboard module function
          (handle-key-internal key state))

         (('modifiers keyboard serial mods-depressed mods-latched mods-locked group)
          ;; Update XKB state using existing keyboard module function
          (when (xkb-state)
            (xkb-state-update-mask (xkb-state)
                                   mods-depressed
                                   mods-latched
                                   mods-locked
                                   0 0 group)))

         (('repeat-info keyboard rate delay)
          ;; Handle repeat info
          #t))

       (loop)))))

;; Complete setup function
(define (setup-keyboard-fiber keyboard)
  (let ((kbd-context (make-keyboard-fiber-context
                      (make-channel)  ; keymap
                      (make-channel)  ; enter
                      (make-channel)  ; leave
                      (make-channel)  ; key
                      (make-channel)  ; modifiers
                      (make-channel))))  ; repeat-info

    ;; Add the keyboard listener
    (wl-keyboard-add-listener keyboard (create-fiber-keyboard-listener kbd-context))

    ;; Start the keyboard event processor fiber
    (keyboard-event-processor kbd-context)

    kbd-context))
