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
            parse-hex-color))

(define WL_SHM_FORMAT_ARGB8888 0)

;; Cairo font settings
(define font-face "Sans")
(define font-size 14)
(define background-color (make-parameter '(0.1 0.1 0.1)))
(define foreground-color (make-parameter '(0.9 0.9 0.9)))
(define selected-color '(0.4 0.4 0.8))
(define input-color '(1.0 0.8 0.2))

;; Parse hex color string (#RRGGBB) to RGB list (0.0-1.0)
(define (parse-hex-color str)
  (let ((s (if (string-prefix? "#" str) (substring str 1) str)))
    (list (/ (string->number (substring s 0 2) 16) 255.0)
          (/ (string->number (substring s 2 4) 16) 255.0)
          (/ (string->number (substring s 4 6) 16) 255.0))))

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

;; Draw the menu with all options
(define (draw-menu width height padding cache
                   prompt input-text selected-index
                   filtered-options max-options)
  (let* ((item-height (+ font-size (* 2 padding)))
         (real-height (* (+ 1 (min (length filtered-options) max-options)) item-height))
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

             ;; Set background
             (apply cairo-set-source-rgb cr (background-color))
             (cairo-rectangle cr 0 0 width real-height)
             (cairo-fill cr)

             ;; Set font
             (cairo-select-font-face cr font-face 'normal 'normal)
             (cairo-set-font-size cr font-size)

             ;; Calculate the text extents for the prompt
             (let* ((text-extents (cairo-text-extents cr prompt))
                    (prompt-width (cairo-text-extents:width text-extents))
                    (prompt-x-advance (cairo-text-extents:x-advance text-extents))
                    ;; Position after the prompt with a small gap
                    (x-position (+ padding prompt-x-advance 2)))

               ;; Draw input prompt
               (apply cairo-set-source-rgb cr (foreground-color))
               (cairo-move-to cr padding (/ (+ font-size item-height) 2))
               (cairo-show-text cr prompt)

               ;; Draw input text
               (apply cairo-set-source-rgb cr input-color)
               (cairo-move-to cr x-position (/ (+ font-size item-height) 2))
               (cairo-show-text cr input-text)

               ;; Draw cursor at end of input
               (let* ((input-extents (cairo-text-extents cr input-text))
                      (input-width (cairo-text-extents:width input-extents))
                      (cursor-x (+ x-position input-width 2)))
                (apply cairo-set-source-rgb cr (foreground-color))
                (cairo-rectangle cr cursor-x padding 2 (- item-height (/ (* 3 padding) 2)))
                (cairo-fill cr))

               ;; Draw menu items
               (let loop ((items filtered-options)
                          (index 0))
                 (when (and (not (null? items)) (< index max-options))
                   (let ((y (+ item-height (* index item-height))))
                     ;; Draw selection background if this is the selected item
                     (when (= index selected-index)
                       (apply cairo-set-source-rgb cr selected-color)
                       (cairo-rectangle cr 0 y width item-height)
                       (cairo-fill cr))
                     ;; Draw item text
                     (apply cairo-set-source-rgb cr (foreground-color))
                     (cairo-move-to cr x-position (+ y (/ item-height 2) (/ font-size 2)))
                     (cairo-show-text cr (car items))
                     (loop (cdr items) (+ index 1))))))

             ;; Clean up Cairo resources
             (cairo-destroy cr)
             (cairo-surface-destroy surface))

           buffer))))
