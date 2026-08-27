(define-module (guile-dmenu graphics)
  #:use-module (guile-dmenu memory-utils)
  #:use-module (wayland client protocol wayland)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (cairo)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:export (draw-menu
            make-menu-buffer-cache
            wl-buffer-listener
            background-color
            foreground-color
            highlight-background-color
            highlight-foreground-color
            filter-background-color
            filter-foreground-color
            cursor-color
            title-background-color
            title-foreground-color
            alt-background-color
            alt-foreground-color
            border-color
            border-width
            line-height
            fixed-height?
            prefix-text
            item-height
            visible-item-count
            menu-height
            parse-hex-color))

(define WL_SHM_FORMAT_ARGB8888 0)

;; Cairo font settings
(define font-face "Sans")
(define font-size 14)

;; Colors. Naming follows bemenu's flag names (nb/nf, hb/hf, fb/ff, ...) so
;; options are trivially portable. Parameters left #f fall back to
;; foreground-color/background-color at draw time, which keeps default
;; rendering identical to before these were introduced.
(define background-color (make-parameter '(0.1 0.1 0.1)))         ; --nb
(define foreground-color (make-parameter '(0.9 0.9 0.9)))         ; --nf
(define highlight-background-color (make-parameter '(0.4 0.4 0.8))) ; --hb
(define highlight-foreground-color (make-parameter '(0.9 0.9 0.9))) ; --hf
(define filter-background-color (make-parameter #f))              ; --fb
(define filter-foreground-color (make-parameter '(1.0 0.8 0.2)))  ; --ff
(define cursor-color (make-parameter #f))                         ; --cb
(define title-background-color (make-parameter #f))               ; --tb
(define title-foreground-color (make-parameter #f))               ; --tf
(define alt-background-color (make-parameter #f))                 ; --ab
(define alt-foreground-color (make-parameter #f))                 ; --af
(define border-color (make-parameter '(0.3 0.3 0.3)))             ; --bdr
(define border-width (make-parameter 0))                          ; --border

;; Layout / behavior
(define line-height (make-parameter #f))    ; --line-height (#f = auto)
(define fixed-height? (make-parameter #f))  ; --fixed-height
(define prefix-text (make-parameter ""))    ; --prefix, shown before the
                                             ; selected item

;; Parse hex color string (#RRGGBB) to RGB list (0.0-1.0)
(define (parse-hex-color str)
  (let ((s (if (string-prefix? "#" str) (substring str 1) str)))
    (list (/ (string->number (substring s 0 2) 16) 255.0)
          (/ (string->number (substring s 2 4) 16) 255.0)
          (/ (string->number (substring s 4 6) 16) 255.0))))

;; Height (in pixels) of a single prompt/item row.
(define (item-height padding)
  (or (line-height) (+ font-size (* 2 padding))))

;; Number of item rows actually drawn, given how many options matched and
;; the configured cap. With fixed-height?, always reserves room for
;; max-options rows so the window doesn't resize as the filter narrows.
(define (visible-item-count option-count max-options)
  (if (fixed-height?)
      max-options
      (min option-count max-options)))

;; Total menu height (prompt row + item rows).
(define (menu-height padding option-count max-options)
  (* (+ 1 (visible-item-count option-count max-options))
     (item-height padding)))

;; Buffer listener for handling release events
(define wl-buffer-listener
  (make <wl-buffer-listener>
    #:release (lambda (data buffer)
                (wl-buffer-destroy buffer))))

(define-record-type <menu-buffer-cache>
  (%make-menu-buffer-cache shm buffer-count buffers retired-buffers
                           width height pending-redraw? request-redraw)
  menu-buffer-cache?
  (shm menu-buffer-cache-shm)
  (buffer-count menu-buffer-cache-buffer-count)
  (buffers menu-buffer-cache-buffers set-menu-buffer-cache-buffers!)
  (retired-buffers menu-buffer-cache-retired-buffers
                   set-menu-buffer-cache-retired-buffers!)
  (width menu-buffer-cache-width set-menu-buffer-cache-width!)
  (height menu-buffer-cache-height set-menu-buffer-cache-height!)
  (pending-redraw? menu-buffer-cache-pending-redraw?
                   set-menu-buffer-cache-pending-redraw?!)
  (request-redraw menu-buffer-cache-request-redraw))

(define-record-type <menu-buffer>
  (%make-menu-buffer wl-buffer data width height stride size
                     released? retired? destroyed? listener)
  menu-buffer?
  (wl-buffer menu-buffer-wl-buffer)
  (data menu-buffer-data)
  (width menu-buffer-width)
  (height menu-buffer-height)
  (stride menu-buffer-stride)
  (size menu-buffer-size)
  (released? menu-buffer-released? set-menu-buffer-released?!)
  (retired? menu-buffer-retired? set-menu-buffer-retired?!)
  (destroyed? menu-buffer-destroyed? set-menu-buffer-destroyed?!)
  ;; Keep the listener object alive for as long as libwayland can call its FFI
  ;; trampoline through the raw listener pointer registered on this wl_buffer.
  (listener menu-buffer-listener set-menu-buffer-listener!))

(define* (make-menu-buffer-cache shm #:key (buffer-count 2)
                                 (request-redraw (lambda () #t)))
  (%make-menu-buffer-cache shm buffer-count '() '() #f #f #f request-redraw))

(define (destroy-menu-buffer! menu-buffer)
  (when (and (menu-buffer-released? menu-buffer)
             (not (menu-buffer-destroyed? menu-buffer)))
    (wl-buffer-destroy (menu-buffer-wl-buffer menu-buffer))
    (munmap (menu-buffer-data menu-buffer))
    (set-menu-buffer-destroyed?! menu-buffer #t)))

(define (retire-menu-buffer! menu-buffer)
  (set-menu-buffer-retired?! menu-buffer #t)
  (destroy-menu-buffer! menu-buffer))

;; Create and return a reusable Wayland SHM buffer.
(define (create-buffer cache width height)
  (let* ((stride (* 4 width))
         (size (* stride height))
         (fd (memfd-create "guile-wl-dmenu" 1))
         (_ (truncate-file fd size))
         (data (mmap #f size (logior PROT_READ PROT_WRITE) MAP_SHARED fd 0))
         (pool (wl-shm-create-pool (menu-buffer-cache-shm cache) fd size))
         (buffer (wl-shm-pool-create-buffer
                  pool 0
                  width height stride
                  WL_SHM_FORMAT_ARGB8888))
         (menu-buffer (%make-menu-buffer buffer data width height stride size
                                         #t #f #f #f))
         (listener
          (make <wl-buffer-listener>
            #:release
            (lambda (data buffer)
              (set-menu-buffer-released?! menu-buffer #t)
              (when (menu-buffer-retired? menu-buffer)
                (destroy-menu-buffer! menu-buffer))
              (when (menu-buffer-cache-pending-redraw? cache)
                (set-menu-buffer-cache-pending-redraw?! cache #f)
                ((menu-buffer-cache-request-redraw cache)))
              #t))))

    ;; Clean up pool and file descriptor
    (wl-shm-pool-destroy pool)
    (close-fdes fd)

    (set-menu-buffer-listener! menu-buffer listener)
    (wl-buffer-add-listener buffer listener)
    menu-buffer))

(define (make-buffer-list cache width height count)
  (let loop ((remaining count)
             (buffers '()))
    (if (zero? remaining)
        buffers
        (loop (- remaining 1)
              (cons (create-buffer cache width height) buffers)))))

(define (ensure-menu-buffers! cache width height)
  (unless (and (equal? (menu-buffer-cache-width cache) width)
               (equal? (menu-buffer-cache-height cache) height))
    (for-each retire-menu-buffer! (menu-buffer-cache-buffers cache))
    (set-menu-buffer-cache-retired-buffers!
     cache
     (append (menu-buffer-cache-buffers cache)
             (menu-buffer-cache-retired-buffers cache)))
    (set-menu-buffer-cache-width! cache width)
    (set-menu-buffer-cache-height! cache height)
    (set-menu-buffer-cache-buffers!
     cache
     (make-buffer-list cache width height
                       (menu-buffer-cache-buffer-count cache)))))

(define (next-menu-buffer cache width height)
  (ensure-menu-buffers! cache width height)
  (let loop ((buffers (menu-buffer-cache-buffers cache)))
    (match buffers
      (() (set-menu-buffer-cache-pending-redraw?! cache #t)
          #f)
      ((buffer . rest)
       (if (and (menu-buffer-released? buffer)
                (not (menu-buffer-destroyed? buffer)))
           (begin
             (set-menu-buffer-released?! buffer #f)
             buffer)
           (loop rest))))))

;; Render prompt/input/items onto an already-created Cairo context. Pure
;; drawing logic, kept separate from buffer/wayland plumbing so it can be
;; exercised directly (e.g. against an in-memory surface) without a
;; compositor.
(define (draw-menu-to-cairo-context cr width real-height padding
                                    prompt input-text selected-index
                                    filtered-options max-options)
  (let* ((row-height (item-height padding))
         (tb (title-background-color))
         (tf (or (title-foreground-color) (foreground-color)))
         (fb (filter-background-color))
         (ff (filter-foreground-color))
         (cb (or (cursor-color) (foreground-color)))
         (ab (alt-background-color))
         (af (or (alt-foreground-color) (foreground-color)))
         (hb (highlight-background-color))
         (hf (highlight-foreground-color))
         (bw (border-width)))

    ;; Set background
    (apply cairo-set-source-rgb cr (background-color))
    (cairo-rectangle cr 0 0 width real-height)
    (cairo-fill cr)

    ;; Set font
    (cairo-select-font-face cr font-face 'normal 'normal)
    (cairo-set-font-size cr font-size)

    ;; Calculate the text extents for the prompt
    (let* ((text-extents (cairo-text-extents cr prompt))
           (prompt-x-advance (cairo-text-extents:x-advance text-extents))
           ;; Position after the prompt with a small gap
           (x-position (+ padding prompt-x-advance 2)))

      ;; Filter row background (input line), if configured
      (when fb
        (apply cairo-set-source-rgb cr fb)
        (cairo-rectangle cr 0 0 width row-height)
        (cairo-fill cr))

      ;; Title/prompt badge background, if configured
      (when tb
        (apply cairo-set-source-rgb cr tb)
        (cairo-rectangle cr 0 0 x-position row-height)
        (cairo-fill cr))

      ;; Draw input prompt
      (apply cairo-set-source-rgb cr tf)
      (cairo-move-to cr padding (/ (+ font-size row-height) 2))
      (cairo-show-text cr prompt)

      ;; Draw input text
      (apply cairo-set-source-rgb cr ff)
      (cairo-move-to cr x-position (/ (+ font-size row-height) 2))
      (cairo-show-text cr input-text)

      ;; Draw cursor at end of input
      (let* ((input-extents (cairo-text-extents cr input-text))
             (input-width (cairo-text-extents:width input-extents))
             (cursor-x (+ x-position input-width 2)))
        (apply cairo-set-source-rgb cr cb)
        (cairo-rectangle cr cursor-x padding 2 (- row-height (/ (* 3 padding) 2)))
        (cairo-fill cr))

      ;; Draw menu items
      (let loop ((items filtered-options)
                 (index 0))
        (when (and (not (null? items)) (< index max-options))
          (let ((y (+ row-height (* index row-height)))
                (selected? (= index selected-index))
                (odd-row? (odd? index)))
            ;; Row background: selection takes priority over alternating rows
            (cond (selected?
                   (apply cairo-set-source-rgb cr hb)
                   (cairo-rectangle cr 0 y width row-height)
                   (cairo-fill cr))
                  ((and ab odd-row?)
                   (apply cairo-set-source-rgb cr ab)
                   (cairo-rectangle cr 0 y width row-height)
                   (cairo-fill cr)))
            ;; Item text, optionally prefixed on the selected item
            (apply cairo-set-source-rgb cr (cond (selected? hf)
                                                  ((and ab odd-row?) af)
                                                  (else (foreground-color))))
            (cairo-move-to cr x-position (+ y (/ row-height 2) (/ font-size 2)))
            (cairo-show-text cr (string-append (if selected? (prefix-text) "")
                                                (car items)))
            (loop (cdr items) (+ index 1))))))

    ;; Border, drawn last so it isn't painted over
    (when (> bw 0)
      (apply cairo-set-source-rgb cr (border-color))
      (cairo-set-line-width cr bw)
      (cairo-rectangle cr (/ bw 2) (/ bw 2) (- width bw) (- real-height bw))
      (cairo-stroke cr))))

;; Draw the menu with all options
(define (draw-menu width height padding cache
                   prompt input-text selected-index
                   filtered-options max-options)
  (let* ((real-height (menu-height padding (length filtered-options) max-options))
         (menu-buffer (next-menu-buffer cache width real-height)))

    (and menu-buffer
         (let ((stride (menu-buffer-stride menu-buffer))
               (data (menu-buffer-data menu-buffer))
               (buffer (menu-buffer-wl-buffer menu-buffer)))

           ;; Draw to the buffer using Cairo
           (let* ((surface (cairo-image-surface-create-for-data
                            data
                            'argb32
                            width
                            real-height
                            stride))
                  (cr (cairo-create surface)))

             (draw-menu-to-cairo-context cr width real-height padding
                                         prompt input-text selected-index
                                         filtered-options max-options)

             ;; Clean up Cairo resources
             (cairo-destroy cr)
             (cairo-surface-destroy surface))

           buffer))))
