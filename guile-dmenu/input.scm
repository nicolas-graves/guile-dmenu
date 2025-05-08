(define-module (guile-dmenu input)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (xkbcommon keysyms)
  #:export (make-input-handler
            filter-options
            select-option))

;; Filter options based on input text
(define (filter-options options input)
  (if (string-null? input)
      options
      (filter
       (lambda (opt)
         (string-contains-ci opt input))
       options)))

;; Select option and exit with result
(define (select-option filtered-options selected-index)
  (when (and (not (null? filtered-options))
             (< selected-index (length filtered-options)))
    (let ((selected (list-ref filtered-options selected-index)))
      (format #t "~a~%" selected)
      (exit 0))))

;; Create an input handler function
(define (make-input-handler on-change input-text selected-index
                            filtered-options options max-options)
  (lambda (key xkb-state)
    (let* ((xkb-key (+ 8 key))
           (keysym (xkb-state-key-get-one-sym xkb-state xkb-key))
           (ctrl-pressed (not (zero? (logand (xkb-state-mod-name-is-active
                                              xkb-state
                                              "Control"
                                              XKB_STATE_MODS_EFFECTIVE)
                                             1)))))
      (cond
       ;; ESC - Exit program
       ((= keysym XKB_KEY_Escape)
        (exit 1))

       ;; Ctrl-c/g - Exit program
       ((and ctrl-pressed (memq keysym (list XKB_KEY_c XKB_KEY_g)))
        (exit 1))

       ;; Enter - Select current option
       ((= keysym XKB_KEY_Return)
        (select-option (filtered-options) (selected-index)))

       ;; Down arrow - Move selection down
       ((= keysym XKB_KEY_Down)
        (let* ((filtered (filtered-options))
               (current (selected-index))
               (new-idx (if (< (+ current 1) (length filtered))
                            (+ current 1)
                            current)))
          (selected-index new-idx)
          (on-change)))

       ;; Up arrow - Move selection up
       ((= keysym XKB_KEY_Up)
        (let* ((current (selected-index))
               (new-idx (if (> current 0) (- current 1) 0)))
          (selected-index new-idx)
          (on-change)))

       ;; Backspace - Delete last character of input
       ((= keysym XKB_KEY_BackSpace)
        (let ((current-text (input-text)))
          (unless (string-null? current-text)
            (input-text (substring current-text 0 (- (string-length current-text) 1)))
            (filtered-options (filter-options (options) (input-text)))
            (selected-index 0)
            (on-change))))

       ;; Regular character input
       (else
        (unless ctrl-pressed
          (let ((utf32 (xkb-state-key-get-utf32 xkb-state xkb-key)))
            (when (and (>= utf32 0) (<= utf32 #xD7FF)) ; Valid Unicode range
              (let* ((char (integer->char utf32))
                     (current-text (input-text)))
                (input-text (string-append current-text (string char)))
                (filtered-options (filter-options (options) (input-text)))
                (selected-index 0)
                (on-change))))))))))
