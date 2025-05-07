(define-module (guile-dmenu-package)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (guile-wayland packages guile-wayland)
  #:use-module (guile-wayland packages guile-xyz))

(define-public guile-bytestructure-class-next
  ((package-input-rewriting/spec
    `(("guile" . ,(const guile-next))))
   (package/inherit guile-bytestructure-class
     (arguments
      ;; XXX: guile-next breaks one test.
      (list #:tests? #f
            #:make-flags #~'("GUILE_AUTO_COMPILE=0"))))))

(define next
  (package-input-rewriting/spec
   `(("guile" . ,(const guile-next))
     ("guile-bytestructure-class" . ,(const guile-bytestructure-class-next))
     ;; Don't try to rebuild those.
     ("swig" . ,(const (@ (gnu packages swig) swig)))
     ("graphviz" . ,(const (@ (gnu packages graphviz) graphviz))))))

(match (cadr (command-line))
  ("build" guile-wayland)
  ("shell" (packages->manifest
            (append (map next
                         (list guile-cairo
                               guile-wayland
                               guile-xkbcommon))
                    (list guile-next
                          guile-ares-rs
                          guile-bytestructure-class-next))))
  (_ (package->development-manifest guile-wayland)))
