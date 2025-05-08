(define-module (guile-dmenu graphics)
  #:use-module (guile-dmenu memory-utils)
  #:use-module (wayland client protocol wayland)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (cairo)
  #:use-module (oop goops)
  #:export (draw-menu
            wl-buffer-listener))

(define WL_SHM_FORMAT_ARGB8888 0)

;; Cairo font settings
(define font-face "Sans")
(define font-size 14)
(define background-color '(0.1 0.1 0.1))
(define foreground-color '(0.9 0.9 0.9))
(define selected-color '(0.4 0.4 0.8))
(define input-color '(1.0 0.8 0.2))

;; Buffer listener for handling release events
(define wl-buffer-listener
  (make <wl-buffer-listener>
    #:release (lambda (data buffer)
                (wl-buffer-destroy buffer))))

;; Create and return a Wayland buffer
(define (create-buffer shm width height)
  (let* ((stride (* 4 width))
         (size (* stride height))
         (fd (memfd-create "guile-wl-dmenu" 1))
         (_ (truncate-file fd size))
         (data (mmap #f size (logior PROT_READ PROT_WRITE) MAP_SHARED fd 0))
         (pool (wl-shm-create-pool shm fd size))
         (buffer (wl-shm-pool-create-buffer
                  pool 0
                  width height stride
                  WL_SHM_FORMAT_ARGB8888)))

    ;; Clean up pool and file descriptor
    (wl-shm-pool-destroy pool)
    (close-fdes fd)

    ;; Return both the buffer and the data for drawing
    (cons buffer data)))

;; Draw the menu with all options
(define (draw-menu width height padding shm
                   prompt input-text selected-index
                   filtered-options max-options)
  (let* ((item-height (+ font-size (* 2 padding)))
         (real-height (* (+ 1 (min (length filtered-options) max-options)) item-height))
         (stride (* 4 width))
         (size (* stride real-height))
         (buffer-data (create-buffer shm width real-height))
         (buffer (car buffer-data))
         (data (cdr buffer-data)))

    ;; Draw to the buffer using Cairo
    (let* ((surface (cairo-image-surface-create-for-data
                     data
                     'argb32
                     width
                     real-height
                     stride))
           (cr (cairo-create surface)))

      ;; Set background
      (apply cairo-set-source-rgb cr background-color)
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
        (apply cairo-set-source-rgb cr foreground-color)
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
          (apply cairo-set-source-rgb cr foreground-color)
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
              (apply cairo-set-source-rgb cr foreground-color)
              (cairo-move-to cr x-position (+ y (/ item-height 2) (/ font-size 2)))
              (cairo-show-text cr (car items))
              (loop (cdr items) (+ index 1))))))

      ;; Clean up Cairo resources
      (cairo-destroy cr)
      (cairo-surface-destroy surface))

    ;; Clean up memory but keep buffer
    (munmap data)

    ;; Add listener to buffer and return it
    (wl-buffer-add-listener buffer wl-buffer-listener)
    buffer))
