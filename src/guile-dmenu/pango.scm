(define-module (guile-dmenu pango)
  #:use-module (g-golf)
  #:use-module (cairo)
  #:use-module (cairo config)
  #:use-module (system foreign)
  #:export (default-font-pixel-size
            pango-text-size
            pango-text-width
            draw-pango-text!))

;; Import the introspected Pango layout API and its Cairo rendering bridge.
(gi-import "Pango")
(gi-import "PangoCairo")

;; Keep the current presentation unchanged while moving all text through
;; Pango.  Font configuration remains deliberately out of scope for this
;; migration; the px suffix preserves Cairo's former pixel size.
(define default-font-face "Sans")
(define default-font-pixel-size 14)

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
                  (number->string default-font-pixel-size) "px")))

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
