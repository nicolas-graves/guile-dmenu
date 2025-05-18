(define-module (guile-dmenu-package)
  #:use-module (ice-9 match)
  #:use-module (guix build-system guile)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
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

(define-public guile-dmenu
  (let ((commit "58b61fd923af9d4299150de3e70fa4f5943ed341")
        (revision "8"))
    (package
      (name "guile-dmenu")
      (version (git-version "0.0.0" revision commit))
      (source
       (git-checkout
        (url "https://git.sr.ht/~ngraves/guile-dmenu")
        (commit commit)))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "src"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'install-script
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((bin (string-append #$output "/bin/"))
                       (dmenu (string-append bin "dmenu")))
                  (install-file "scripts/dmenu" bin)
                  (wrap-program dmenu
                    #:sh (search-input-file inputs "bin/bash")
                    `("GUILE_AUTO_COMPILE" ":" = ("0"))
                    `("GUILE_LOAD_PATH" ":" prefix
                      ,(string-split (getenv "GUILE_LOAD_PATH") #\:))
                    `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                      ,(string-split (getenv "GUILE_LOAD_COMPILED_PATH")
                                     #\:)))))))))
      (native-inputs (list guile-3.0))
      (inputs (list bash-minimal guile-3.0))
      (propagated-inputs (list guile-cairo
                               guile-fibers
                               guile-wayland
                               guile-xkbcommon))
      (home-page "https://git.sr.ht/~ngraves/guile-dmenu")
      (synopsis "Guile completing-read library and dynamic menu")
      (description "This package provides a guile wayland implementation
for dmenu.  On the guile-side, you can also find an Emacs-like
completing-read procedure.")
      (license license:gpl3+))))

(match (cadr (command-line))
  ("build" guile-dmenu)
  ("shell" (concatenate-manifests
            (list (package->development-manifest (next guile-dmenu))
                  (packages->manifest
                   (list guile-next
                         guile-ares-rs
                         guile-bytestructure-class-next))))))
