(define-module (guile-dmenu keyboard)
  #:use-module (guile-dmenu memory-utils)
  #:use-module (wayland client protocol wayland)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (xkbcommon keysyms)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (set-key-handler!
            set-key-event-channel!
            handle-key-internal
            process-keymap
            wl-keyboard-listener
            wl-seat-listener
            wl-keyboard-listener-with-fibers
            wl-seat-listener-with-fibers
            make-key-decoder))

(define-record-type <keymap-state>
  (make-keymap-state context keymap xkb-state) keymap-state?
  (context keymap-state-context)
  (keymap keymap-state-keymap)
  (xkb-state keymap-state-xkb-state))

(define keymap-state (make-parameter #f))
(define key-event-channel (make-parameter #f))
(define key-handler (make-parameter #f))

(define (set-key-handler! handler)
  (key-handler handler))

(define (set-key-event-channel! channel)
  (key-event-channel channel))

;; Repeat timing (in seconds)
(define repeat-delay (make-parameter 0.5))
(define repeat-rate (make-parameter 0.05))

(define (log . args)
  (apply format (current-error-port) args)
  (force-output (current-error-port)))

;; --- Key decoding ---

(define (process-keymap format fd size)
  "Process a keymap received from the compositor and spawn key event handler."
  (and (> size 0)
       (let* ((ctx (xkb-context-new))
              (data (mmap #f size PROT_READ MAP_SHARED fd 0))
              (keymap-string (utf8->string data))
              (km (xkb-keymap-new ctx keymap-string)))
         (munmap data)
         (keymap-state (make-keymap-state ctx km (xkb-state-new km)))
         (spawn-fiber
          (lambda ()
            (let loop ((repeat-key #f) (first-repeat? #f))
              (match (perform-operation
                      (choice-operation
                       (get-operation (key-event-channel))
                       (wrap-operation
                        (sleep-operation (if first-repeat?
                                             (repeat-delay)
                                             (repeat-rate)))
                        (lambda () 'timeout))))
                ;; Key press
                (('key key 1 time)
                 (and (key-handler) ((key-handler) key))
                 (loop key #t))  ; Start with initial delay

                ;; Key release
                (('key key 0 time)
                 (loop #f #f))

                ;; Modifier change
                (('modifiers depressed latched locked group)
                 (and=> (keymap-state-xkb-state (keymap-state))
                        (cut xkb-state-update-mask
                             <> depressed latched locked 0 0 group))
                 (loop repeat-key first-repeat?))

                ;; Timeout - repeat the action if we're repeating
                ('timeout
                 (and=> repeat-key (key-handler))
                 (loop repeat-key #f))  ; After first repeat, use normal rate

                (other
                 (log "Unhandled key event: ~a~%" other)
                 (loop #f #f)))))))))

;; Fiber-aware keyboard listener
(define wl-keyboard-listener-with-fibers
  (make <wl-keyboard-listener>
    #:keymap
    (lambda (data keyboard format fd size)
      (process-keymap format fd size)
      (when (> fd 0)
        (close-fdes fd)))

    #:enter
    (lambda (data keyboard serial surface keys)
      #t)

    #:leave
    (lambda (data keyboard serial surface)
      #t)

    #:key
    (lambda (data keyboard serial time key state)
      (log "Key event: key=~a state=~a~%" key state)
      ;; We can't use put-message directly here due to continuation barrier
      ;; Instead, spawn a fiber to do it
      (spawn-fiber
       (lambda ()
         (put-message (key-event-channel) `(key ,key ,state ,time))))
      #t)

    #:modifiers
    (lambda (data keyboard serial mods-depressed mods-latched mods-locked group)
      ;; Spawn a fiber to put the message
      (spawn-fiber
       (lambda ()
         (put-message (key-event-channel)
                      `(modifiers ,mods-depressed ,mods-latched ,mods-locked ,group))))
      #t)

    #:repeat-info
    (lambda (data keyboard rate delay)
      #t)))

;; Fiber-aware seat listener
(define wl-seat-listener-with-fibers
  (make <wl-seat-listener>
    #:capabilities
    (lambda (data seat capabilities)
      ;; Check if keyboard capability is available (bit 1)
      (when (logand capabilities 2)
        (wl-keyboard-add-listener (wl-seat-get-keyboard seat)
                                  wl-keyboard-listener-with-fibers))
      #t)

    #:name
    (lambda (data seat name)
      #t)))

;; Function to set the key handler
(define (make-key-decoder app-channel exit-channel)
  (lambda (key)
    (let* ((xkb-key (+ 8 key))
           (xkb-state (keymap-state-xkb-state (keymap-state)))
           (keysym (xkb-state-key-get-one-sym xkb-state xkb-key))
           (ctrl-pressed (not (zero? (logand (xkb-state-mod-name-is-active
                                              xkb-state
                                              "Control"
                                              XKB_STATE_MODS_EFFECTIVE)
                                             1)))))

      (cond
       ;; ESC/Ctrl+g/c - Exit program
       ((or (= keysym XKB_KEY_Escape)
            (and ctrl-pressed (memq keysym (list XKB_KEY_c XKB_KEY_g))))
        (put-message exit-channel 1))

       ;; Enter - Select current option
       ((= keysym XKB_KEY_Return)
        (put-message app-channel 'select))

       ;; Down arrow/Ctrl+n - Move selection down
       ((or (= keysym XKB_KEY_Down)
            (and ctrl-pressed (= keysym XKB_KEY_n)))
        (put-message app-channel 'next))

       ;; Up arrow/Ctrl+p - Move selection up
       ((or (= keysym XKB_KEY_Up)
            (and ctrl-pressed (= keysym XKB_KEY_p)))
        (put-message app-channel 'previous))

       ;; Backspace - Delete last character
       ((= keysym XKB_KEY_BackSpace)
        (put-message app-channel 'backspace))

       ;; Character input - Add character
       (else
        (unless ctrl-pressed
          (let ((utf32 (xkb-state-key-get-utf32 xkb-state xkb-key)))
            (when (and (> utf32 0) (<= utf32 #xD7FF)) ; Valid Unicode range
              (put-message app-channel `(input-char ,(integer->char utf32))))))))
      #t)))
