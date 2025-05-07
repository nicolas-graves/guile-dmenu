(use-modules (guile-dmenu memory-utils)
             (wayland client display)
             (wayland client protocol wayland)
             (wayland client protocol xdg-shell)
             (oop goops)
             (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (rnrs bytevectors)
             (xkbcommon xkbcommon)
             (xkbcommon keysyms)
             (cairo))

;; Prevent GC to avoid potential segment faults during drawing
(gc-disable)

;; Parameters to store important objects
(define compositor (make-parameter #f))
(define shm (make-parameter #f))
(define xdg-wm-base (make-parameter #f))
(define xdg-toplevel (make-parameter #f))
(define wsurface (make-parameter #f))
(define seat (make-parameter #f))
(define keyboard (make-parameter #f))
(define width* (make-parameter 800))
(define options* (make-parameter '()))
(define max-options* (make-parameter 10))
(define prompt* (make-parameter "dmenu: "))
(define selected-index* (make-parameter 0))
(define input-text* (make-parameter ""))
(define filtered-options* (make-parameter '()))

;; XKB state for handling key translation
(define xkb-context (make-parameter #f))
(define xkb-keymap (make-parameter #f))
(define xkb-state (make-parameter #f))

;; Buffer creation and drawing configuration
(define PROT_READ 1)
(define PROT_WRITE 2)
(define MAP_SHARED 1)
(define WL_SHM_FORMAT_ARGB8888 0)

;; Cairo font settings
(define font-face "Sans")
(define font-size 14)
(define background-color '(0.1 0.1 0.1))
(define foreground-color '(0.9 0.9 0.9))
(define selected-color '(0.4 0.4 0.8))
(define input-color '(1.0 0.8 0.2))
(define item-padding 8)
(define item-height (+ font-size (* 2 item-padding)))

(define wl-buffer-listener
  (make <wl-buffer-listener>
    #:release (lambda (data buffer)
                (wl-buffer-destroy buffer))))

;; Filter options based on input text
(define (filter-options)
  (let ((input (input-text*)))
    (if (string-null? input)
        (filtered-options* (options*))
        (filtered-options* (filter
                           (lambda (opt)
                             (string-contains-ci opt input))
                           (options*))))))

;; Select option and exit with result
(define (select-option)
  (let* ((filtered (filtered-options*))
         (idx (selected-index*)))
    (when (and (not (null? filtered))
               (< idx (length filtered)))
      (let ((selected (list-ref filtered idx)))
        (format #t "~a~%" selected)
        (exit 0)))))

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

;; Handle keyboard key events
(define (handle-key key state)
  (when (= state 1) ; Key pressed
    (let* ((xkb-key (+ 8 key))
           (keysym (xkb-state-key-get-one-sym (xkb-state) xkb-key)))
      (cond
       ;; ESC - Exit program
       ((= keysym XKB_KEY_Escape)
        (exit 1))

       ;; Enter - Select current option
       ((= keysym XKB_KEY_Return)
        (select-option))

       ;; Down arrow - Move selection down
       ((= keysym XKB_KEY_Down)
        (let* ((filtered (filtered-options*))
               (current (selected-index*))
               (new-idx (if (< (+ current 1) (length filtered))
                            (+ current 1)
                            current)))
          (selected-index* new-idx)
          (redraw)))

       ;; Up arrow - Move selection up
       ((= keysym XKB_KEY_Up)
        (let* ((current (selected-index*))
               (new-idx (if (> current 0) (- current 1) 0)))
          (selected-index* new-idx)
          (redraw)))

       ;; Backspace - Delete last character of input
       ((= keysym XKB_KEY_BackSpace)
        (let ((current-text (input-text*)))
          (unless (string-null? current-text)
            (input-text* (substring current-text 0 (- (string-length current-text) 1)))
            (filter-options)
            (selected-index* 0)
            (redraw))))

       ;; Regular character input
       (else
        (let ((name (pk 'name (xkb-keysym-get-name (pk 'keysym keysym))))
              (utf32 (xkb-state-key-get-utf32 (xkb-state) xkb-key)))
          (when (and (>= utf32 0) (<= utf32 #xD7FF)) ; Valid Unicode range
            (let* ((char (pk 'char (integer->char utf32)))
                   (current-text (input-text*)))
              (input-text* (pk 'input (string-append current-text (string char))))
              (filter-options)
              (selected-index* 0)
              (redraw)))))))))

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

         ;; Create rule names with user's layout
         (names (make <xkb-rule-names>
                  #:rules "evdev"
                  #:model "pc105"
                  #:layout lang)))

    (xkb-context ctx)
    (let ((km (xkb-keymap-new ctx names)))
      (xkb-keymap km)
      (xkb-state (xkb-state-new km)))))

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
      (handle-key key state))

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
        (let ((kb (wl-seat-get-keyboard seat)))
          (keyboard kb)
          (wl-keyboard-add-listener kb wl-keyboard-listener))))

    #:name
    (lambda (data seat name)
      #t)))

;; Draw the menu
(define (draw-frame)
  (let* ((width (width*))
         (height (* (+ 1 (min (length (filtered-options*)) (max-options*))) item-height))
         (stride (* 4 width))
         (size (* stride height))
         (fd (memfd-create "guile-wl-dmenu" 1))
         (_ (truncate-file fd size))
         (data (mmap #f size (logior PROT_READ PROT_WRITE) MAP_SHARED fd 0))
         (pool (wl-shm-create-pool (shm) fd size))
         (buffer (wl-shm-pool-create-buffer
                  pool 0
                  width height stride
                  WL_SHM_FORMAT_ARGB8888)))

    ;; Draw to the buffer using Cairo
    (let* ((surface (cairo-image-surface-create-for-data
                     data
                     'argb32
                     width
                     height
                     stride))
           (cr (cairo-create surface)))

      ;; Set background
      (apply cairo-set-source-rgb cr background-color)
      (cairo-rectangle cr 0 0 width height)
      (cairo-fill cr)

      ;; Set font
      (cairo-select-font-face cr font-face 'normal 'normal)
      (cairo-set-font-size cr font-size)

      ;; Draw input prompt
      (apply cairo-set-source-rgb cr foreground-color)
      (cairo-move-to cr item-padding (/ (+ font-size item-height) 2))
      (cairo-show-text cr (prompt*))

      ;; Use fixed position after prompt
      (let ((x-position (+ item-padding 100))) ; Fixed offset for simplicity

        ;; Draw input text
        (apply cairo-set-source-rgb cr input-color)
        (cairo-move-to cr x-position (/ (+ font-size item-height) 2))
        (cairo-show-text cr (input-text*))

        ;; Draw cursor at end of input (simple approach)
        ;; (let ((cursor-x (+ x-position (* (string-length (input-text*)) (/ font-size 1.5)))))
        ;;   (apply cairo-set-source-rgb cr foreground-color)
        ;;   (cairo-rectangle cr cursor-x (item-padding) 2 (- item-height (* 2 item-padding)))
        ;;   (cairo-fill cr))
        )

      ;; Draw menu items
      (let loop ((items (filtered-options*))
                 (index 0))
        (when (and (not (null? items))
                   (< index (max-options*)))
          (let ((y (+ item-height (* index item-height))))
            ;; Draw selection background if this is the selected item
            (when (= index (selected-index*))
              (apply cairo-set-source-rgb cr selected-color)
              (cairo-rectangle cr 0 y width item-height)
              (cairo-fill cr))

            ;; Draw item text
            (if (= index (selected-index*))
                (apply cairo-set-source-rgb cr foreground-color)
                (apply cairo-set-source-rgb cr foreground-color))
            (cairo-move-to cr item-padding (+ y (/ item-height 2) (/ font-size 2)))
            (cairo-show-text cr (car items))
            (loop (cdr items) (+ index 1)))))

      ;; Clean up Cairo resources
      (cairo-destroy cr)
      (cairo-surface-destroy surface))

    ;; Clean up pool and memory
    (wl-shm-pool-destroy pool)
    (close-fdes fd)
    (munmap data)

    ;; Add listener to buffer and return it
    (wl-buffer-add-listener buffer wl-buffer-listener)
    buffer))

;; Force a redraw of the window
(define (redraw)
  (when (and (wsurface) (shm))
    (let ((buffer (draw-frame)))
      (wl-surface-attach (wsurface) buffer 0 0)
      (wl-surface-commit (wsurface)))))

(define (read-stdin)
  (let loop ((line (read-line)))
      (unless (eof-object? line)
        (options* (append (options*) (list line)))
        (loop (read-line)))))

;; Main function
(define (main . args)
  (read-stdin)

  ;; Initialize filtered options
  (filtered-options* (options*))

  ;; Initialize XKB for keyboard handling
  (initialize-fallback-keymap)

  ;; Connect to Wayland display
  (let* ((w-display (wl-display-connect)))
    (unless w-display
      (format (current-error-port) "Unable to connect to wayland compositor~%")
      (exit -1))

    ;; Get registry and set up listeners
    (let ((registry (wl-display-get-registry w-display))
          (listener (make <wl-registry-listener>
                      #:global
                      (lambda (data registry name interface version)
                        (match interface
                          ("wl_compositor"
                           (compositor (wrap-wl-compositor
                                        (wl-registry-bind
                                         registry name
                                         %wl-compositor-interface 3))))
                          ("wl_shm"
                           (shm (wrap-wl-shm
                                 (wl-registry-bind registry name %wl-shm-interface 1))))
                          ("xdg_wm_base"
                           (xdg-wm-base
                            (wrap-xdg-wm-base
                             (wl-registry-bind registry name %xdg-wm-base-interface 1)))
                           (xdg-wm-base-add-listener
                            (xdg-wm-base)
                            (make <xdg-wm-base-listener>
                              #:ping (lambda (data base serial)
                                       (xdg-wm-base-pong base serial)))))
                          ("wl_seat"
                           (seat (wrap-wl-seat
                                  (wl-registry-bind registry name %wl-seat-interface 7)))
                           (wl-seat-add-listener (seat) wl-seat-listener))
                          (_
                           #t)))
                      #:global-remove
                      (lambda (data registry name)
                        #t))))

      ;; Add listener to registry
      (wl-registry-add-listener registry listener)
      (wl-display-roundtrip w-display)

      ;; Check if we have all required globals
      (unless (and (compositor) (shm) (xdg-wm-base))
        (format (current-error-port) "Missing required Wayland protocols~%")
        (exit 1))

      ;; Create surface and configure XDG surface
      (let* ((surface (wl-compositor-create-surface (compositor)))
             (xdg-surface (xdg-wm-base-get-xdg-surface (xdg-wm-base) surface)))

        ;; Set up XDG surface listener
        (xdg-surface-add-listener
         xdg-surface
         (make <xdg-surface-listener>
           #:configure
           (lambda (data xdg-surface serial)
             (xdg-surface-ack-configure xdg-surface serial)
             (redraw))))

        ;; Store surface and create XDG toplevel
        (wsurface surface)
        (xdg-toplevel (xdg-surface-get-toplevel xdg-surface))

        ;; Configure toplevel window
        (xdg-toplevel-set-title (xdg-toplevel) "dmenu")
        (xdg-toplevel-set-app-id (xdg-toplevel) "wl-dmenu")

        ;; Set up toplevel listener
        (xdg-toplevel-add-listener
         (xdg-toplevel)
         (make <xdg-toplevel-listener>
           #:configure
           (lambda (data xdg width _height states)
             (pk 'config data xdg width _height states)
             (unless (zero? width)
               (width* width)))
           #:close
           (lambda (data xdg-toplevel)
             (exit 0))))

        ;; Initial draw and commit
        ;; (let ((buffer (draw-frame)))
        ;;   (wl-surface-attach surface buffer 0 0)
        ;;   (wl-surface-commit surface))
        (wl-surface-commit surface)

        ;; Main event loop
        (while (not (zero? (wl-display-dispatch w-display)))
          #t)))))
