(define-module (guile-dmenu pango)
  #:use-module (g-golf)
  #:use-module (cairo)
  #:use-module (cairo config)
  #:use-module (system foreign)
  #:export (default-font-pixel-size
            pango-text-size
            pango-text-width
            pango-wrap-text
            draw-pango-text!))

;; Import the introspected Pango layout API and its Cairo rendering bridge.
(gi-import "Pango")
(gi-import "PangoCairo")

;; Follow the host's monospace Fontconfig alias.  This keeps the menu aligned
;; with terminal and editor font configuration.  Pango's unsuffixed size is in
;; points, matching the size configured by terminal emulators.
(define default-font-face "monospace")
(define default-font-point-size 14)
;; 14 points at Pango's conventional 96 DPI is approximately 19 pixels.  Keep
;; this separately for row geometry, whose dimensions are expressed in pixels.
(define default-font-pixel-size 19)

;; guile-cairo represents cairo_t as a smob, while G-Golf's introspected
;; PangoCairo procedures accept the underlying foreign pointer.  guile-cairo
;; publishes scm_to_cairo for precisely this C-level conversion.
(define scm-to-cairo
  (pointer->procedure '*
                      (dynamic-func "scm_to_cairo"
                                    (dynamic-link *cairo-lib-path*))
                      (list '*)))

(define (cairo-context-pointer cr)
  (scm-to-cairo (scm->pointer cr)))

(define (font-description)
  (pango-font-description-from-string
   (string-append default-font-face " "
                  (number->string default-font-point-size))))

(define (make-text-layout cr text)
  (let ((layout (pango-cairo-create-layout (cairo-context-pointer cr))))
    (pango-layout-set-font-description layout (font-description))
    ;; The introspected API retains Pango's explicit byte-length argument.
    (pango-layout-set-text layout text -1)
    layout))

(define (pango-text-size cr text)
  (let ((layout (make-text-layout cr text)))
    (call-with-values (lambda () (pango-layout-get-pixel-size layout)) list)))

(define (pango-text-width cr text)
  (car (pango-text-size cr text)))

(define (pango-wrap-text cr text maximum-width)
  "Wrap TEXT to lines measured with the active Pango font."
  (define (text-width start end)
    (pango-text-width cr (substring text start end)))
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

;; Pango renders a layout from its top-left corner, unlike cairo-show-text,
;; whose current point is a baseline.  Centering by the measured layout height
;; removes the renderer's former hand-written baseline approximations.
(define (draw-pango-text! cr text x row-y row-height)
  (let* ((layout (make-text-layout cr text))
         (size (call-with-values
                   (lambda () (pango-layout-get-pixel-size layout)) list))
         (height (cadr size)))
    (cairo-move-to cr x (+ row-y (max 0 (/ (- row-height height) 2))))
    (pango-cairo-show-layout (cairo-context-pointer cr) layout)))
