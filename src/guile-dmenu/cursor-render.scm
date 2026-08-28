(define-module (guile-dmenu cursor-render)
  #:use-module (cairo)
  #:export (draw-input-cursor!))

;; Draw the cursor at Cairo's pen position for the text before point.  In
;; particular, x-advance includes trailing whitespace while ink width does not.
(define* (draw-input-cursor! cr input cursor-position input-x padding row-height
                             color #:optional (y-offset 0))
  (let* ((before-cursor (substring input 0 cursor-position))
         (extents (cairo-text-extents cr before-cursor))
         (cursor-x (+ input-x (cairo-text-extents:x-advance extents) 2)))
    (apply cairo-set-source-rgb cr color)
    (cairo-rectangle cr cursor-x (+ y-offset padding) 2
                     (- row-height (/ (* 3 padding) 2)))
    (cairo-fill cr)))
