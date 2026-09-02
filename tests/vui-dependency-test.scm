(use-modules (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64)
             (vui)
             (vui guile-wayland)
             (wayland client display))

(test-begin "vui-dependency")

(define vui-source
  (canonicalize-path (search-path %load-path "vui.scm")))
(define wayland-source
  (canonicalize-path
   (search-path %load-path "wayland/client/display.scm")))
(define vui-package (getenv "GUILE_VUI_PACKAGE"))
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
              '("GUILE_DMENU_CHECKOUT" "GUILE_VUI_CHECKOUT"
                "GUILE_WAYLAND_CHECKOUT")))

(test-assert "VUI resolves from a Guix store package"
  (package-provides-source? vui-package vui-source))
(test-assert "generic store root is not exact VUI package provenance"
  (not (package-provides-source? "/gnu/store" vui-source)))
(test-assert "VUI does not resolve from an ambient checkout"
  (every (lambda (prefix) (not (string-prefix? prefix vui-source)))
         forbidden-prefixes))
(test-assert "guile-wayland resolves from a Guix store package"
  (package-provides-source? wayland-package wayland-source))
(test-assert "generic store root is not exact guile-wayland package provenance"
  (not (package-provides-source? "/gnu/store" wayland-source)))
(test-assert "guile-wayland does not resolve from an ambient checkout"
  (every (lambda (prefix) (not (string-prefix? prefix wayland-source)))
         forbidden-prefixes))
(test-equal "validated VUI API version" "0.1.0" (vui-version))
(test-assert "validated Wayland host API is present" (procedure? run-wayland-vui))

(let ((runner (test-runner-current)))
  (test-end "vui-dependency")
  (primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1)))
