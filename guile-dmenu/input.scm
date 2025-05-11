(define-module (guile-dmenu input)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (xkbcommon keysyms)
  #:export (make-input-handler
            filter-options))

;; Filter options based on input text
(define (filter-options options input)
  (if (string-null? input)
      options
      (filter
       (lambda (opt)
         (string-contains-ci opt input))
       options)))

;; Create an input handler function
(define (make-input-handler on-change input-text selected-index
                            filtered-options options max-options
                            request-exit-with)
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
        (request-exit-with 1)
        #t)

       ;; Enter - Select current option
       ((= keysym XKB_KEY_Return)
        (when (and (not (null? (filtered-options)))
                   (< (selected-index) (length (filtered-options))))
          (let ((selected (list-ref (filtered-options) (selected-index))))
            (format #t "~a~%" selected)
            (request-exit-with 0)
            #t)))

       ;; Down arrow - Move selection down
       ((or (= keysym XKB_KEY_Down)
            (and ctrl-pressed (= keysym XKB_KEY_n)))
        (let* ((current (selected-index))
               (new-idx (if (< (+ current 1) (length (filtered-options)))
                            (+ current 1)
                            current)))
          (selected-index new-idx)
          (on-change))
        #t)

       ;; Up arrow - Move selection up
       ((or (= keysym XKB_KEY_Up)
            (and ctrl-pressed (= keysym XKB_KEY_p)))
        (let* ((current (selected-index))
               (new-idx (if (> current 0) (- current 1) 0)))
          (selected-index new-idx)
          (on-change)))

       ;; Backspace - Delete last character of input
       ((= keysym XKB_KEY_BackSpace)
        (let ((current-text (input-text)))
          (unless (string-null? current-text)
            (input-text (pk 'input (string-drop-right current-text 1)))
            (filtered-options (filter-options (options) (input-text)))
            (selected-index 0)
            (on-change))))

       (else
        (unless ctrl-pressed
          (let ((utf32 (xkb-state-key-get-utf32 xkb-state xkb-key)))
            ;; Conveniently special characters have their utf32 equal to 0.
            (cond ((= utf32 0)
                   (pk 'unhandled-keysym (number->string keysym 16)))
                  ;; Regular character input
                  ((and (> utf32 0) (<= utf32 #xD7FF)) ; Valid Unicode range
                   (let* ((char (integer->char utf32))
                          (current-text (input-text)))
                     (input-text (pk 'input (string-append current-text (string char))))
                     (filtered-options (filter-options (options) (input-text)))
                     (selected-index 0)
                     (on-change))))))
        #t)))
    (pk 'ih)
    #t))
