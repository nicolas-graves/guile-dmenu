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
  #:use-module (guile-wayland packages guile-xyz)
  #:use-module (growl-at packages)
  #:use-module (git))

;; G-Golf is Guile-facing, but its test-only native inputs include GTK and an
;; X server.  Letting the broad Guile replacement recurse through those inputs
;; rebuilds their entire closure (including Mesa, librsvg, and Rust tooling)
;; merely because a build tool deep in that graph happens to use Guile.
;; Rebuild only G-Golf and the pure-Scheme guile-lib it directly consumes.
(define-public guile-lib-next
  (package/inherit guile-lib
    (inputs
     (modify-inputs (package-inputs guile-lib)
       (replace "guile" guile-3.0-latest)))
    (native-inputs
     (modify-inputs (package-native-inputs guile-lib)
       (replace "guile" guile-3.0-latest)))))

(define-public guile-g-golf-next
  (package/inherit guile-g-golf
    (inputs
     (modify-inputs (package-inputs guile-g-golf)
       (replace "guile" guile-3.0-latest)
       (replace "guile-lib" guile-lib-next)))))

(define next
  (package-input-rewriting/spec
   `(("guile-g-golf" . ,(const guile-g-golf-next))
     ("guile" . ,(const guile-3.0-latest))
     ("guile-bytestructure-class" . ,(const guile-bytestructure-class-next))
     ("guile-wayland" . ,(const guile-wayland-next))
     ;; Don't try to rebuild those.
     ("swig" . ,(const (@ (gnu packages swig) swig)))
     ("libxkbcommon" . ,(const (@ (gnu packages xdisorg) libxkbcommon)))
     ("graphviz" . ,(const (@ (gnu packages graphviz) graphviz))))))

(define-public guile-dmenu-devel
  (let* ((dir (dirname (repository-discover (dirname (current-filename)))))
         (repo (repository-open dir))
         (head (repository-head repo))
         (oid (reference-target head))
         (commit (oid->string (commit-id (commit-lookup repo oid))))
         (revision "20"))
    (package
      (name "guile-dmenu")
      (version (git-version "0.0.0" revision commit))
      (source
       (git-checkout
        (url dir)
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
                       (dmenu (string-append bin "dmenu"))
                       (approval (string-append bin "codex-dmenu-approval"))
                       (questions (string-append bin "guile-dmenu-questions"))
                       (mcp (string-append bin "guile-dmenu-mcp")))
                  (install-file "scripts/dmenu" bin)
                  (install-file "scripts/codex-dmenu-approval" bin)
                  (install-file "scripts/guile-dmenu-questions" bin)
                  (install-file "scripts/guile-dmenu-mcp" bin)
                  (for-each
                   (lambda (program)
                     (wrap-program program
                       #:sh (search-input-file inputs "bin/bash")
                       `("GUILE_AUTO_COMPILE" ":" = ("0"))
                       `("GUILE_LOAD_PATH" ":" prefix
                         ,(string-split (getenv "GUILE_LOAD_PATH") #\:))
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                         ,(string-split (getenv "GUILE_LOAD_COMPILED_PATH") #\:))
                       `("GI_TYPELIB_PATH" ":" prefix
                         ,(string-split (getenv "GI_TYPELIB_PATH") #\:))
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append #$cairo "/lib")
                          ,(string-append #$pango "/lib")))))
                   (list dmenu approval questions mcp))))))))
      (native-inputs (list guile-3.0))
      (inputs (list bash-minimal guile-3.0))
      (propagated-inputs (list guile-cairo
                               guile-g-golf
                               guile-fibers
                               guile-json-4
                               guile-wayland
                               guile-xkbcommon
                               pango))
      (home-page "https://git.sr.ht/~ngraves/guile-dmenu")
      (synopsis "Guile completing-read library and dynamic menu")
      (description "This package provides a guile wayland implementation
for dmenu.  On the guile-side, you can also find an Emacs-like
completing-read procedure.")
      (license license:gpl3+))))

(match (cadr (command-line))
  ("build" (next guile-dmenu-devel))
  ("shell" (concatenate-manifests
            (list (package->development-manifest (next guile-dmenu))
                  (packages->manifest
                   (list guile-3.0-latest
                         guile-ares-rs
                         guile-bytestructure-class-next))))))
