(use-modules (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64)
             (swing)
             (swing guile-wayland)
             (wayland client display))

(test-begin "swing-dependency")

(define swing-source
  (canonicalize-path (search-path %load-path "swing.scm")))
(define wayland-source
  (canonicalize-path
   (search-path %load-path "wayland/client/display.scm")))
(define swing-package (getenv "GUILE_G_SWING_PACKAGE"))
(define wayland-package (getenv "GUILE_WAYLAND_PACKAGE"))
(define (package-provides-source? package source)
  (and package
       source
       (file-exists? package)
       (let* ((canonical-package (canonicalize-path package))
              (store-prefix "/gnu/store/")
              (store-item (and (string-prefix? store-prefix canonical-package)
                               (substring canonical-package
                                          (string-length store-prefix)))))
         (and store-item
              (not (string-null? store-item))
              (not (string-contains store-item "/"))
              (string-prefix? (string-append canonical-package "/")
                              source)))))
(define forbidden-prefixes
  (filter-map (lambda (name)
                (let ((value (getenv name)))
                  (and value (not (string-null? value)) value)))
              '("GUILE_DMENU_CHECKOUT" "GUILE_G_SWING_CHECKOUT"
                "GUILE_WAYLAND_CHECKOUT")))

(test-assert "Swing resolves from a Guix store package"
  (package-provides-source? swing-package swing-source))
(test-assert "generic store root is not exact Swing package provenance"
  (not (package-provides-source? "/gnu/store" swing-source)))
(test-assert "Swing does not resolve from an ambient checkout"
  (every (lambda (prefix) (not (string-prefix? prefix swing-source)))
         forbidden-prefixes))
(test-assert "guile-wayland resolves from a Guix store package"
  (package-provides-source? wayland-package wayland-source))
(test-assert "generic store root is not exact guile-wayland package provenance"
  (not (package-provides-source? "/gnu/store" wayland-source)))
(test-assert "guile-wayland does not resolve from an ambient checkout"
  (every (lambda (prefix) (not (string-prefix? prefix wayland-source)))
         forbidden-prefixes))
(test-equal "validated Swing API version" "0.1.0" (swing-version))
(test-assert "validated Wayland host API is present" (procedure? run-wayland-swing))

(let ((runner (test-runner-current)))
  (test-end "swing-dependency")
  (primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1)))
