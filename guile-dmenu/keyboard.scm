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
  ;; Add fibers imports
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:export (handle-key-internal
            initialize-fallback-keymap
            process-keymap
            set-key-handler!
            wl-keyboard-listener
            wl-seat-listener
            xkb-context
            xkb-keymap
            xkb-state
            ;; New fiber-related exports
            wl-keyboard-listener-with-fibers
            wl-seat-listener-with-fibers
            initialize-keyboard-fibers
            start-key-processor-fiber))

(define (log . args)
  (apply format (current-error-port) args)
  (force-output (current-error-port)))

;; XKB state for handling key translation
(define xkb-context (make-parameter #f))
(define xkb-keymap (make-parameter #f))
(define xkb-state (make-parameter #f))

;; Fiber-related state
(define key-event-channel (make-parameter #f))

;; Key repeat configuration
(define initial-delay 0.5)
(define repeat-delay 0.05)

;; Initialize fiber support for keyboard
(define (initialize-keyboard-fibers)
  (key-event-channel (make-channel)))

;; Process a keymap received from the compositor
(define (process-keymap format fd size)
  ;; Initialize the XKB context if not already done
  (unless (xkb-context)
    (xkb-context (xkb-context-new)))

  (let* ((ctx (xkb-context))
         (data (mmap #f size PROT_READ MAP_SHARED fd 0))
         (keymap-string (utf8->string data)))

    ;; Create a new keymap from the provided data
    (let ((km (xkb-keymap-new ctx keymap-string)))
      (xkb-keymap km)
      ;; Create a new state object from the keymap
      (xkb-state (xkb-state-new km))
      (munmap data))))

;; Initialize a fallback XKB keymap
(define (initialize-fallback-keymap)
  (let* ((ctx (xkb-context-new))
         ;; Create a rule names structure for the current locale
         (locale (or (getenv "LC_ALL")
                    (getenv "LC_CTYPE")
                    (getenv "LANG")
                    "C"))
         ;; Parse locale to get language code
         (locale-parts (string-split locale #\_))
         (lang (if (>= (length locale-parts) 1)
                  (car locale-parts)
                  "us"))

         ;; Names with user's layout
         (names (make <xkb-rule-names>
                  #:rules "evdev"
                  #:model "pc105"
                  #:layout lang)))

    (xkb-context ctx)
    (let ((km (xkb-keymap-new ctx names)))
      (xkb-keymap km)
      (xkb-state (xkb-state-new km)))))

;; Define a dynamic variable to store key handler
(define key-handler (make-parameter #f))

;; Handle keyboard key events - keep the original signature
(define (handle-key-internal key)
  ;; Call the handler with both key and xkb-state
  (pk 'handled key (and (key-handler) ((key-handler) key (xkb-state)))))

;; Fiber-aware keyboard listener
(define wl-keyboard-listener-with-fibers
  (make <wl-keyboard-listener>
    #:keymap
    (lambda (data keyboard format fd size)
      (process-keymap format fd size)
      (close-fdes fd))

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
(define (set-key-handler! handler)
  (key-handler handler))

(define (key-processor-fiber)
  (let loop ()
    (match (get-message (key-event-channel))
      (('key key 1 time)  ; Key pressed
       (handle-key-internal key))

      (('key key 0 time)  ; Key released
       ;; Later: cancel repeat if it's for this key
       #t)

      (('modifiers mods-depressed mods-latched mods-locked group)
       ;; Update XKB state if available
       (when (xkb-state)
         (xkb-state-update-mask (xkb-state)
                                mods-depressed
                                mods-latched
                                mods-locked
                                0 0 group)))
      (args
       (pk 'unhandled args)))

    (pk 'key-processor-loop)
    (loop))
  #t)

;; Start the key processor fiber
(define (start-key-processor-fiber)
  (format #t "Started the key processor fiber~%")
  (spawn-fiber key-processor-fiber)
  (sleep 0.01))
