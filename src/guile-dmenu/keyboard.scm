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
  #:export (make-keyboard-session
            keyboard-session?
            set-keyboard-session-key-handler!
            process-keymap
            make-keyboard-listener
            make-seat-listener
            make-key-decoder))

(define-record-type <keymap-state>
  (make-keymap-state context keymap xkb-state) keymap-state?
  (context keymap-state-context)
  (keymap keymap-state-keymap)
  (xkb-state keymap-state-xkb-state))

;; Bundles the keyboard-related state for one completing-read session
;; (channel, active handler, decoded keymap, repeat timing) so it can be
;; threaded explicitly instead of living in process-wide parameters.
(define-record-type <keyboard-session>
  (%make-keyboard-session key-event-channel key-handler keymap-state
                           repeat-delay repeat-rate keyboard-listener)
  keyboard-session?
  (key-event-channel keyboard-session-key-event-channel)
  (key-handler keyboard-session-key-handler set-keyboard-session-key-handler!)
  (keymap-state keyboard-session-keymap-state set-keyboard-session-keymap-state!)
  (repeat-delay keyboard-session-repeat-delay)
  (repeat-rate keyboard-session-repeat-rate)
  ;; Kept alive here so its FFI callback trampoline isn't GC'd out from
  ;; under libwayland, which holds only a raw address into this listener.
  (keyboard-listener keyboard-session-keyboard-listener set-keyboard-session-keyboard-listener!))

(define* (make-keyboard-session #:key (repeat-delay 0.5) (repeat-rate 0.05))
  (%make-keyboard-session (make-channel) #f #f repeat-delay repeat-rate #f))

(define (log . args)
  (apply format (current-error-port) args)
  (force-output (current-error-port)))

;; --- Key decoding ---

(define (process-keymap session format fd size)
  "Process a keymap received from the compositor and spawn key event handler."
  (and (> size 0)
       (let* ((ctx (xkb-context-new))
              (data (mmap #f size PROT_READ MAP_SHARED fd 0))
              (keymap-string (utf8->string data))
              (km (xkb-keymap-new ctx keymap-string)))
         (munmap data)
         (set-keyboard-session-keymap-state! session (make-keymap-state ctx km (xkb-state-new km)))
         (spawn-fiber
          (lambda ()
            (let loop ((repeat-key #f) (first-repeat? #f))
              (match (perform-operation
                      (choice-operation
                       (get-operation (keyboard-session-key-event-channel session))
                       (wrap-operation
                        (sleep-operation (if first-repeat?
                                             (keyboard-session-repeat-delay session)
                                             (keyboard-session-repeat-rate session)))
                        (lambda () 'timeout))))
                ;; Key press
                (('key key 1 time)
                 (and (keyboard-session-key-handler session)
                      ((keyboard-session-key-handler session) key))
                 (loop key #t))  ; Start with initial delay

                ;; Key release
                (('key key 0 time)
                 (loop #f #f))

                ;; Modifier change
                (('modifiers depressed latched locked group)
                 (and=> (keymap-state-xkb-state (keyboard-session-keymap-state session))
                        (cut xkb-state-update-mask
                             <> depressed latched locked 0 0 group))
                 (loop repeat-key first-repeat?))

                ;; Timeout - repeat the action if we're repeating
                ('timeout
                 (and=> repeat-key (keyboard-session-key-handler session))
                 (loop repeat-key #f))  ; After first repeat, use normal rate

                (other
                 (log "Unhandled key event: ~a~%" other)
                 (loop #f #f)))))))))

;; Fiber-aware keyboard listener, scoped to a single keyboard session
(define (make-keyboard-listener session)
  (make <wl-keyboard-listener>
    #:keymap
    (lambda (data keyboard format fd size)
      (process-keymap session format fd size)
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
         (put-message (keyboard-session-key-event-channel session) `(key ,key ,state ,time))))
      #t)

    #:modifiers
    (lambda (data keyboard serial mods-depressed mods-latched mods-locked group)
      ;; Spawn a fiber to put the message
      (spawn-fiber
       (lambda ()
         (put-message (keyboard-session-key-event-channel session)
                      `(modifiers ,mods-depressed ,mods-latched ,mods-locked ,group))))
      #t)

    #:repeat-info
    (lambda (data keyboard rate delay)
      #t)))

;; Fiber-aware seat listener, scoped to a single keyboard session
(define (make-seat-listener session)
  (make <wl-seat-listener>
    #:capabilities
    (lambda (data seat capabilities)
      ;; Check if keyboard capability is available (bit 1)
      (when (logand capabilities 2)
        (let ((kl (make-keyboard-listener session)))
          (set-keyboard-session-keyboard-listener! session kl)
          (wl-keyboard-add-listener (wl-seat-get-keyboard seat) kl)))
      #t)

    #:name
    (lambda (data seat name)
      #t)))

;; Function to build the key handler for a session
(define (make-key-decoder session app-channel exit-channel)
  (lambda (key)
    (let* ((xkb-key (+ 8 key))
           (xkb-state (keymap-state-xkb-state (keyboard-session-keymap-state session)))
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

       ;; Ctrl+Backspace - Delete last word
       ((and ctrl-pressed (= keysym XKB_KEY_BackSpace))
        (put-message app-channel 'ctrl+backspace))

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
