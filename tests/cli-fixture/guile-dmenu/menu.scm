(define-module (guile-dmenu menu)
  #:use-module (guile-dmenu graphics)
  #:export (completing-read menu-max-options))

(define menu-max-options (make-parameter 10))

(define* (completing-read prompt collection
                          #:key selection-mode message input-enabled? timeout)
  (display (if (input-line-wrapping?) "input-wrap=enabled\n"
               "input-wrap=disabled\n"))
  #f)
