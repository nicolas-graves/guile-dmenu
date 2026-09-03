;;; Presentation parameters and compositor-independent layout helpers.
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guile-dmenu graphics)
  #:use-module (vui pango)
  #:use-module (srfi srfi-1)
  #:export (wrap-message-lines
            wrap-message-to-width
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

(define background-color (make-parameter '(0.1 0.1 0.1)))
(define foreground-color (make-parameter '(0.9 0.9 0.9)))
(define highlight-background-color (make-parameter '(0.4 0.4 0.8)))
(define highlight-foreground-color (make-parameter '(0.9 0.9 0.9)))
(define filter-background-color (make-parameter #f))
(define filter-foreground-color (make-parameter '(1.0 0.8 0.2)))
(define cursor-color (make-parameter #f))
(define title-background-color (make-parameter #f))
(define title-foreground-color (make-parameter #f))
(define alt-background-color (make-parameter #f))
(define alt-foreground-color (make-parameter #f))
(define border-color (make-parameter '(0.3 0.3 0.3)))
(define border-width (make-parameter 0))

(define line-height (make-parameter #f))
(define fixed-height? (make-parameter #f))
(define input-line-wrapping? (make-parameter #f))
(define prefix-text (make-parameter ""))

(define %default-font-pixel-size 19)
(define %pango-service (make-pango-service "monospace 14"))

(define (parse-hex-color str)
  (let ((s (if (string-prefix? "#" str) (substring str 1) str)))
    (list (/ (string->number (substring s 0 2) 16) 255.0)
          (/ (string->number (substring s 2 4) 16) 255.0)
          (/ (string->number (substring s 4 6) 16) 255.0))))

(define (item-height padding)
  (or (line-height) (+ %default-font-pixel-size (* 2 padding))))

(define (visible-item-count option-count max-options)
  (if (fixed-height?)
      max-options
      (min option-count max-options)))

(define* (menu-height padding option-count max-options
                      #:optional (message-lines 0) (input-lines 1)
                      (option-lines #f))
  (* (+ input-lines message-lines
        (or option-lines (visible-item-count option-count max-options)))
     (item-height padding)))

(define (available-text-width width padding)
  (max 1 (- width padding (max padding (border-width)))))

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

(define (text-width text)
  (text-measurement-width (pango-measure %pango-service text)))

;; Produce the strings consumed by the VUI tree while using the same shaping
;; service as its renderer. Prefer whitespace boundaries, but always make
;; progress for paths and other unbroken strings.
(define (wrap-line-to-width line maximum-width)
  (define length (string-length line))
  (define (fits? start end)
    (<= (text-width (substring line start end)) maximum-width))
  (define (fitted-position start)
    (let loop ((low (+ start 1)) (high length) (best start))
      (if (> low high)
          (max (+ start 1) best)
          (let ((middle (quotient (+ low high) 2)))
            (if (fits? start middle)
                (loop (+ middle 1) high middle)
                (loop low (- middle 1) best))))))
  (define (word-boundary start fitted)
    (if (= fitted length)
        fitted
        (let loop ((position fitted))
          (cond ((<= position (+ start 1)) fitted)
                ((char-whitespace? (string-ref line (- position 1)))
                 (- position 1))
                (else (loop (- position 1)))))))
  (define (skip-whitespace position)
    (if (and (< position length)
             (char-whitespace? (string-ref line position)))
        (skip-whitespace (+ position 1))
        position))
  (if (zero? length)
      (list "")
      (let loop ((start 0) (result '()))
        (let* ((fitted (fitted-position start))
               (end (word-boundary start fitted))
               (next (skip-whitespace
                      (if (< end fitted) (+ end 1) fitted)))
               (result (cons (substring line start end) result)))
          (if (>= next length)
              (reverse result)
              (loop next result))))))

(define* (wrap-message-to-width message width padding #:optional (maximum 12))
  (let* ((raw (append-map
               (lambda (line)
                 (wrap-line-to-width
                  line (available-text-width width padding)))
               (string-split (or message "") #\newline)))
         (truncated? (> (length raw) maximum)))
    (if truncated?
        (append (take raw (max 0 (- maximum 1)))
                (list "... [truncated]"))
        raw)))

(define (menu-visible-window options selected-index max-options)
  (let* ((option-count (length options))
         (page-start (if (positive? max-options)
                         (* (quotient selected-index max-options) max-options)
                         0))
         (start (min page-start (max 0 (- option-count max-options))))
         (count (min max-options (- option-count start))))
    (list (take (drop options start) count)
          (- selected-index start))))

