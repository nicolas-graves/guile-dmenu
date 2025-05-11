(define-module (guile-dmenu input)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (xkbcommon keysyms)
  #:use-module (fibers channels)
  #:use-module (fibers)
  #:export (make-key-decoder
            filter-options))

;; Filter options based on input text
(define (filter-options options input)
  (if (string-null? input)
      options
      (filter
       (lambda (opt)
         (string-contains-ci opt input))
       options)))

(define (make-key-decoder state-channel exit-channel)
  (lambda (key xkb-state)
    (let* ((xkb-key (+ 8 key))
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
        (put-message state-channel 'select))

       ;; Down arrow/Ctrl+n - Move selection down
       ((or (= keysym XKB_KEY_Down)
            (and ctrl-pressed (= keysym XKB_KEY_n)))
        (put-message state-channel 'move-down))

       ;; Up arrow/Ctrl+p - Move selection up
       ((or (= keysym XKB_KEY_Up)
            (and ctrl-pressed (= keysym XKB_KEY_p)))
        (put-message state-channel 'move-up))

       ;; Backspace - Delete last character
       ((= keysym XKB_KEY_BackSpace)
        (put-message state-channel 'backspace))

       ;; Character input - Add character
       (else
        (unless ctrl-pressed
          (let ((utf32 (xkb-state-key-get-utf32 xkb-state xkb-key)))
            (when (and (> utf32 0) (<= utf32 #xD7FF)) ; Valid Unicode range
              (put-message state-channel `(input-char ,(integer->char utf32))))))))
      #t)))
