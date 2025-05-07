(define-module (guile-dmenu keyboard)
  #:use-module (guile-dmenu memory-utils)
  #:use-module (wayland client protocol wayland)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (xkbcommon keysyms)
  #:export (handle-key-internal
            initialize-fallback-keymap
            process-keymap
            set-key-handler!
            wl-keyboard-listener
            wl-seat-listener
            xkb-context
            xkb-keymap
            xkb-state))

;; XKB state for handling key translation
(define xkb-context (make-parameter #f))
(define xkb-keymap (make-parameter #f))
(define xkb-state (make-parameter #f))

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

;; Handle keyboard key events
(define (handle-key-internal key state)
  (when (and (= state 1) (key-handler)) ; Key pressed and handler set
    ((key-handler) key)))

;; Set up keyboard listener
(define wl-keyboard-listener
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
      (handle-key-internal key state))

    #:modifiers
    (lambda (data keyboard serial mods-depressed mods-latched mods-locked group)
      ;; Update the XKB state with the current modifier state
      (when (xkb-state)
        (xkb-state-update-mask (xkb-state)
                              mods-depressed
                              mods-latched
                              mods-locked
                              0 0 group)))

    #:repeat-info
    (lambda (data keyboard rate delay)
      #t)))

;; Set up seat listener
(define wl-seat-listener
  (make <wl-seat-listener>
    #:capabilities
    (lambda (data seat capabilities)
      ;; Check if keyboard capability is available (bit 1)
      (when (logand capabilities 2)
        (wl-keyboard-add-listener (wl-seat-get-keyboard seat)
                                  wl-keyboard-listener)))

    #:name
    (lambda (data seat name)
      #t)))

;; Function to set the key handler
(define (set-key-handler! handler)
  (key-handler handler))
