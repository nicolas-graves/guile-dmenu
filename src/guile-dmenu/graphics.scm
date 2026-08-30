(define-module (guile-dmenu graphics)
  #:use-module (guile-dmenu cursor-render)
  #:use-module (guile-dmenu memory-utils)
  #:use-module (wayland client protocol wayland)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (cairo)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (draw-menu
            draw-menu-to-cairo-context
            wrap-message-lines
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
            input-line-wrapping?
            prefix-text
            item-height
            visible-item-count
            menu-height
            menu-visible-window
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
(define input-line-wrapping? (make-parameter #f))
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
(define* (menu-height padding option-count max-options
                      #:optional (message-lines 0) (input-lines 1)
                      (option-lines #f))
  (* (+ input-lines message-lines
        (or option-lines (visible-item-count option-count max-options)))
     (item-height padding)))

(define (available-columns width padding)
  (max 1 (quotient (max 1 (- width (* 2 padding))) 8)))

(define (wrap-text cr text maximum-width)
  (define (text-width start end)
    (cairo-text-extents:x-advance
     (cairo-text-extents cr (substring text start end))))
  (define (fit-position start)
    (let fit ((position (+ start 1)))
      (cond ((> position (string-length text)) (string-length text))
            ((<= (text-width start position) maximum-width)
             (fit (+ position 1)))
            (else (max (+ start 1) (- position 1))))))
  (define (break-position start fitted)
    (let find ((position fitted))
      (cond ((<= position start) fitted)
            ((char-whitespace? (string-ref text (- position 1)))
             (if (= position (+ start 1)) fitted (- position 1)))
            (else (find (- position 1))))))
  (define (skip-whitespace position)
    (if (and (< position (string-length text))
             (char-whitespace? (string-ref text position)))
        (skip-whitespace (+ position 1))
        position))
  (let loop ((start 0) (lines '()))
    (if (= start (string-length text))
        (reverse (if (null? lines) (list "") lines))
        (let ((fitted (fit-position start)))
          (if (= fitted (string-length text))
              (reverse (cons (substring text start fitted) lines))
              (let ((end (break-position start fitted)))
                (loop (skip-whitespace end)
                      (cons (substring text start end) lines))))))))

;; Return editable text rows as (START . TEXT) pairs.  The first row shares
;; horizontal space with the prompt; continuation rows use the full width.
(define (input-layout prompt input-text width padding)
  (if (not (input-line-wrapping?))
      (list (cons 0 input-text))
      (let* ((columns (available-columns width padding))
             (first-columns (max 1 (- columns (string-length prompt) 1)))
             (length (string-length input-text)))
        (define (break-position start limit)
          (let find ((position limit))
            (cond ((<= position (+ start 1)) limit)
                  ((char-whitespace?
                    (string-ref input-text (- position 1)))
                   position)
                  (else (find (- position 1))))))
        (let loop ((start 0) (capacity first-columns) (rows '()))
          (let* ((limit (min length (+ start capacity)))
                 (end (if (= limit length)
                          limit
                          (break-position start limit)))
                 (next (cons (cons start (substring input-text start end)) rows)))
            (if (= end length)
                (reverse next)
                (loop end columns next)))))))

(define (option-layout cr options selected-index width padding)
  (map (lambda (option index)
         (let ((text (string-append (if (= index selected-index)
                                        (prefix-text)
                                        "")
                                    option)))
           (if (input-line-wrapping?)
               (wrap-text cr text (max 1 (- width (* 2 padding))))
               (list text))))
       options
       (iota (length options))))

;; Wrap text at word boundaries when possible, while still breaking commands,
;; paths, and serialized inputs that contain no whitespace.
(define* (wrap-message-lines message columns #:optional (maximum 12))
  (define (chunks s)
    (define (break-position)
      (let find ((position columns))
        (cond ((zero? position) columns)
              ((char-whitespace? (string-ref s (- position 1)))
               (- position 1))
              (else (find (- position 1))))))
    (define (skip-whitespace position)
      (if (and (< position (string-length s))
               (char-whitespace? (string-ref s position)))
          (skip-whitespace (+ position 1))
          position))
    (if (<= (string-length s) columns)
        (list s)
        (let* ((end (break-position))
               (next (skip-whitespace
                      (if (= end columns) end (+ end 1)))))
          (cons (substring s 0 end) (chunks (substring s next))))))
  (let* ((raw (append-map chunks (string-split (or message "") #\newline)))
         (truncated? (> (length raw) maximum)))
    (if truncated?
        (append (take raw (max 0 (- maximum 1))) (list "... [truncated]"))
        raw)))

;; Return the page of OPTIONS containing SELECTED-INDEX, together with the
;; selection index relative to that page.  Keeping this calculation in the
;; renderer means the completion state continues to use an absolute index.
(define (menu-visible-window options selected-index max-options)
  (let* ((option-count (length options))
         (page-start (if (positive? max-options)
                         (* (quotient selected-index max-options) max-options)
                         0))
         ;; Fill the final page when possible instead of leaving unused rows.
         (start (min page-start (max 0 (- option-count max-options))))
         (count (min max-options (- option-count start))))
    (list (take (drop options start) count)
          (- selected-index start))))

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

(define (destroy-menu-buffer! cache menu-buffer)
  (when (and (menu-buffer-released? menu-buffer)
             (not (menu-buffer-destroyed? menu-buffer)))
    (wl-buffer-destroy (menu-buffer-wl-buffer menu-buffer))
    (munmap (menu-buffer-data menu-buffer))
    (set-menu-buffer-destroyed?! menu-buffer #t)
    (set-menu-buffer-cache-retired-buffers!
     cache
     (delq menu-buffer (menu-buffer-cache-retired-buffers cache)))))

(define (retire-menu-buffer! cache menu-buffer)
  (set-menu-buffer-retired?! menu-buffer #t)
  (destroy-menu-buffer! cache menu-buffer))

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
                (destroy-menu-buffer! cache menu-buffer))
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
    (let ((old-buffers (menu-buffer-cache-buffers cache)))
      ;; Register old buffers as retired before destroying released ones, so
      ;; destroy-menu-buffer! can remove them from the cache immediately.
      (set-menu-buffer-cache-retired-buffers!
       cache
       (append old-buffers (menu-buffer-cache-retired-buffers cache)))
      (for-each (lambda (buffer) (retire-menu-buffer! cache buffer))
                old-buffers)
      (set-menu-buffer-cache-width! cache width)
      (set-menu-buffer-cache-height! cache height)
      (set-menu-buffer-cache-buffers!
       cache
       (make-buffer-list cache width height
                         (menu-buffer-cache-buffer-count cache))))))

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
(define* (draw-menu-to-cairo-context cr width real-height padding
                                     prompt input-text selected-index
                                     filtered-options max-options
                                     #:key (message-lines '()) (input-enabled? #t)
                                     (cursor-position (string-length input-text)))
  (let* ((row-height (item-height padding))
         (input-rows (input-layout prompt input-text width padding))
         (input-row-count (length input-rows))
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
        (cairo-rectangle cr 0 0 width (* input-row-count row-height))
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

      ;; Draw editable text, with continuation rows when wrapping is enabled.
      (let loop ((rows input-rows) (index 0))
        (unless (null? rows)
          (let* ((row (car rows))
                 (start (car row))
                 (text (cdr row))
                 (row-x (if (zero? index) x-position padding)))
            (apply cairo-set-source-rgb cr ff)
            (cairo-move-to cr row-x
                           (+ (* index row-height)
                              (/ (+ font-size row-height) 2)))
            (cairo-show-text cr text)
            (when (and input-enabled?
                       (<= start cursor-position)
                       (or (< cursor-position (+ start (string-length text)))
                           (and (= cursor-position
                                   (+ start (string-length text)))
                                (null? (cdr rows)))))
              (draw-input-cursor! cr text (- cursor-position start)
                                  row-x padding row-height cb
                                  (* index row-height)))
            (loop (cdr rows) (+ index 1)))))

      ;; Read-only detail panel.
      (let loop ((lines message-lines) (index 0))
        (unless (null? lines)
          (apply cairo-set-source-rgb cr (foreground-color))
          (cairo-move-to cr padding
                         (+ (* (+ index input-row-count) row-height)
                            (/ row-height 2) (/ font-size 2)))
          (cairo-show-text cr (car lines))
          (loop (cdr lines) (+ index 1))))

      ;; Draw the page containing the selected option.
      (match (menu-visible-window filtered-options selected-index max-options)
        ((visible-options visible-selected-index)
         (let loop ((items (option-layout cr visible-options
                                          visible-selected-index width padding))
                    (index 0)
                    (line-offset 0))
           (unless (null? items)
             (let* ((lines (car items))
                    (line-count (length lines))
                    (y (* (+ input-row-count (length message-lines) line-offset)
                          row-height))
                   (selected? (= index visible-selected-index))
                   (odd-row? (odd? index)))
               ;; Row background: selection takes priority over alternating rows
               (cond (selected?
                      (apply cairo-set-source-rgb cr hb)
                      (cairo-rectangle cr 0 y width (* line-count row-height))
                      (cairo-fill cr))
                     ((and ab odd-row?)
                      (apply cairo-set-source-rgb cr ab)
                      (cairo-rectangle cr 0 y width (* line-count row-height))
                      (cairo-fill cr)))
               ;; Item text, optionally prefixed on the selected item
               (apply cairo-set-source-rgb cr (cond (selected? hf)
                                                     ((and ab odd-row?) af)
                                                     (else (foreground-color))))
               ;; Options are rows of their own, not continuations of the
               ;; prompt/input row.  Starting them at x-position made a long
               ;; prompt consume (and sometimes visually clip) their text.
               (let draw-lines ((remaining lines) (line-index 0))
                 (unless (null? remaining)
                   (cairo-move-to cr padding
                                  (+ y (* line-index row-height)
                                     (/ row-height 2) (/ font-size 2)))
                   (cairo-show-text cr (car remaining))
                   (draw-lines (cdr remaining) (+ line-index 1))))
               (loop (cdr items) (+ index 1) (+ line-offset line-count))))))))

    ;; Border, drawn last so it isn't painted over
    (when (> bw 0)
      (apply cairo-set-source-rgb cr (border-color))
      (cairo-set-line-width cr bw)
      (cairo-rectangle cr (/ bw 2) (/ bw 2) (- width bw) (- real-height bw))
      (cairo-stroke cr))))

;; Draw the menu with all options
(define* (draw-menu width height padding cache
                   prompt input-text selected-index
                   filtered-options max-options
                   #:key (message-lines '()) (input-enabled? #t)
                   (scale 1)
                   (cursor-position (string-length input-text)))
  (let* ((option-line-count
          (let* ((surface (cairo-image-surface-create 'argb32 1 1))
                 (cr (cairo-create surface)))
            (cairo-select-font-face cr font-face 'normal 'normal)
            (cairo-set-font-size cr font-size)
            (let ((count
                   (match (menu-visible-window filtered-options selected-index
                                               max-options)
                     ((visible-options visible-selected-index)
                      (fold + 0
                            (map length
                                 (option-layout cr visible-options
                                                visible-selected-index
                                                width padding)))))))
              (cairo-destroy cr)
              (cairo-surface-destroy surface)
              count)))
         (real-height (menu-height padding (length filtered-options) max-options
                                  (length message-lines)
                                  (length (input-layout prompt input-text
                                                        width padding))
                                  option-line-count))
         (buffer-width (* scale width))
         (buffer-height (* scale real-height))
         (menu-buffer (next-menu-buffer cache buffer-width buffer-height)))

    (and menu-buffer
         (let ((stride (menu-buffer-stride menu-buffer))
               (data (menu-buffer-data menu-buffer))
               (buffer (menu-buffer-wl-buffer menu-buffer)))

           ;; Draw to the buffer using Cairo
           (let* ((surface (cairo-image-surface-create-for-data
                            data
                            'argb32
                            buffer-width
                            buffer-height
                            stride))
                  (cr (cairo-create surface)))

             ;; Wayland buffers use physical pixels while all menu layout is
             ;; expressed in logical surface coordinates.
             (cairo-scale cr scale scale)
             (draw-menu-to-cairo-context cr width real-height padding
                                         prompt input-text selected-index
                                         filtered-options max-options
                                         #:message-lines message-lines
                                         #:cursor-position cursor-position
                                         #:input-enabled? input-enabled?)

             ;; Clean up Cairo resources
             (cairo-destroy cr)
             (cairo-surface-destroy surface))

           buffer))))
