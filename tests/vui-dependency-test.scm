(use-modules (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64)
             (vui)
             (vui guile-wayland)
             (wayland client))

(test-begin "vui-dependency")

(define vui-source (search-path %load-path "vui.scm"))
(define wayland-source (search-path %load-path "wayland/client.scm"))
(define vui-package (getenv "GUILE_VUI_PACKAGE"))
(define wayland-package (getenv "GUILE_WAYLAND_PACKAGE"))
(define forbidden-prefixes
  (filter-map (lambda (name)
                (let ((value (getenv name)))
                  (and value (not (string-null? value)) value)))
              '("GUILE_DMENU_CHECKOUT" "GUILE_VUI_CHECKOUT"
                "GUILE_WAYLAND_CHECKOUT")))

(test-assert "VUI resolves from a Guix store package"
  (and vui-package vui-source (string-prefix? vui-package vui-source)))
(test-assert "VUI does not resolve from an ambient checkout"
  (every (lambda (prefix) (not (string-prefix? prefix vui-source)))
         forbidden-prefixes))
(test-assert "guile-wayland resolves from a Guix store package"
  (and wayland-package wayland-source
       (string-prefix? wayland-package wayland-source)))
(test-assert "guile-wayland does not resolve from an ambient checkout"
  (every (lambda (prefix) (not (string-prefix? prefix wayland-source)))
         forbidden-prefixes))
(test-equal "validated VUI API version" "0.1.0" (vui-version))
(test-assert "validated Wayland host API is present" (procedure? run-wayland-vui))

(test-end "vui-dependency")
