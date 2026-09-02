(define-module (guile-dmenu-package)
  #:use-module (ice-9 match)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages zig-xyz)
  #:use-module (guile-wayland packages guile-wayland)
  #:use-module (guile-wayland packages guile-xyz)
  #:use-module (growl-at packages)
  #:use-module (git))

(define %source-directory
  (or (getenv "GUILE_DMENU_PROJECT_DIR")
      (dirname (dirname (dirname (current-filename))))))

(define (dependency-source environment-name expected-commit)
  (let* ((source (getenv environment-name))
         (_ (unless (and source (not (string-null? source)))
              (error (format #f
                             "~a must name the controller-provided dependency source"
                             environment-name))))
         (repository (repository-open source))
         (head (repository-head repository))
         (actual-commit
          (oid->string
           (commit-id
            (commit-lookup repository (reference-target head))))))
    (unless (string=? actual-commit expected-commit)
      (error (format #f
                     "~a must resolve to pinned revision ~a, not ~a"
                     environment-name expected-commit actual-commit)))
    source))

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

;; Pin the validated host and binding as one dependency graph.  In particular,
;; neither origin may fall back to a neighbouring development checkout.
(define %guile-wayland-commit
  "35eff2a28f9843dbdddeee2cc594788b72569bad")

(define* (normalize-git-checkout checkout hash-algo hash
                                 #:optional name
                                 #:key (system (%current-system)))
  (gexp->derivation
   (or name "git-checkout")
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (copy-recursively #$checkout #$output)))
   #:system system
   #:hash-algo hash-algo
   #:hash hash
   #:recursive? #t
   #:local-build? #t))

(define (dependency-origin source commit hash name)
  ;; An explicit content hash makes this a fixed-output derivation.  Thus the
  ;; store identity depends on the pinned tree, not on the controller's local
  ;; checkout path (or on mutable files in that checkout).
  (origin
    (method normalize-git-checkout)
    (uri (git-checkout
          (url source)
          (commit commit)))
    (file-name name)
    (sha256 (base32 hash))))

(define-public guile-wayland-dmenu
  (package/inherit guile-wayland-next
    (version (git-version "0.0.2" "1" %guile-wayland-commit))
    (source
     (let ((source (dependency-source "GUILE_WAYLAND_SOURCE"
                                      %guile-wayland-commit)))
       (dependency-origin
        source %guile-wayland-commit
        "0ykrcv5py9ig6m9jfxm9dnygwc24qli1ii866yfvigsx7jjz2dsl"
        (git-file-name "guile-wayland" %guile-wayland-commit))))
    (arguments
     (cons* #:strip-binaries? #f
            (substitute-keyword-arguments (package-arguments guile-wayland-next)
              ((#:phases phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'disable-incompatible-callback-helper
                     (lambda _
                       ;; This pinned bytestructure-class release exports the
                       ;; helper but listener classes have no %keep-alive slot.
                       ;; The scanner's following strong root table retains
                       ;; both closures and trampoline pointers itself.
                       (substitute* "modules/wayland/scanner.scm"
                         (("\\(bs-keep-alive! obj callback\\)")
                          "#t")))))))))))

(define %guile-vui-commit
  "b35ea090d22d4e821950e180e06ada58b06714b7")

(define-public guile-vui-dmenu
  (package
    (name "guile-vui")
    (version (git-version "0.1.0" "1" %guile-vui-commit))
    (source
     (let ((source (dependency-source "GUILE_VUI_SOURCE"
                                      %guile-vui-commit)))
       (dependency-origin
        source %guile-vui-commit
        "1p5shsi06ls112wvk7vdvxv5fhwl92vqa961gbr2pv5vr2mxsqc6"
        (git-file-name "guile-vui" %guile-vui-commit))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "prefix=" #$output)
                            "GUILE_AUTO_COMPILE=0")
      #:strip-binaries? #f
      #:modules '((guix build gnu-build-system)
                  ((guix build utils) #:hide (delete)))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure (const #t))
          (replace 'build
            (lambda* (#:key make-flags #:allow-other-keys)
              (setenv "LC_ALL" "C")
              (setenv "SOURCE_DATE_EPOCH" "1")
              (setenv "GUILE_AUTO_COMPILE" "0")
              (apply invoke "make" "-j1" "all" make-flags)))
          (replace 'check
            (lambda* (#:key tests? make-flags #:allow-other-keys)
              (when tests?
                (setenv "XDG_CACHE_HOME" (string-append (getcwd) "/.cache"))
                (mkdir-p (getenv "XDG_CACHE_HOME"))
                (apply invoke "make" "-j1" "check" make-flags))))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "-j1" "install" make-flags))))))
    (native-inputs (list guile-3.0-latest))
    (propagated-inputs
     (list guile-3.0-latest guile-cairo guile-fibers guile-g-golf
           guile-wayland-dmenu libxkbcommon cairo pango))
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/3.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/3.0/site-ccache")))
           (search-path-specification
            (variable "LD_LIBRARY_PATH")
            (files '("lib")))))
    (home-page "https://codeberg.org/igorge/guile-vui")
    (synopsis "Declarative user-interface core for Guile")
    (description "Guile-VUI provides a deterministic, compositor-independent
declarative user-interface core for GNU Guile.")
    (license license:gpl3+)))

(define next
  (package-input-rewriting/spec
   `(("guile-g-golf" . ,(const guile-g-golf-next))
     ("guile" . ,(const guile-3.0-latest))
     ("guile-bytestructure-class" . ,(const guile-bytestructure-class-next))
     ("guile-wayland" . ,(const guile-wayland-dmenu))
     ;; Don't try to rebuild those.
     ("swig" . ,(const (@ (gnu packages swig) swig)))
     ("libxkbcommon" . ,(const (@ (gnu packages xdisorg) libxkbcommon)))
     ("graphviz" . ,(const (@ (gnu packages graphviz) graphviz))))))

(define-public guile-dmenu-devel
  (let* ((dir %source-directory)
         (tracked? (git-predicate dir))
         (d1.2-source-files
          (map (lambda (file) (string-append dir "/" file))
               '("src/guile-dmenu/vui-adapter.scm"
                 "tests/vui-adapter-test.scm")))
         (repo (repository-open dir))
         (head (repository-head repo))
         (oid (reference-target head))
         (commit (oid->string (commit-id (commit-lookup repo oid))))
         (revision "20"))
    (package
      (name "guile-dmenu")
      (version (git-version "0.0.0" revision commit))
      (source
       (local-file %source-directory "guile-dmenu-checkout"
                   #:recursive? #t
                   ;; git-predicate reflects HEAD, so explicitly retain the
                   ;; bounded D1.2 additions before the controller commits.
                   #:select? (lambda (file stat)
                               (or (tracked? file stat)
                                   (member file d1.2-source-files)))))
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
                               guile-vui-dmenu
                               guile-wayland-dmenu
                               guile-xkbcommon
                               pango))
      (home-page "https://git.sr.ht/~ngraves/guile-dmenu")
      (synopsis "Guile completing-read library and dynamic menu")
      (description "This package provides a guile wayland implementation
for dmenu.  On the guile-side, you can also find an Emacs-like
completing-read procedure.")
      (license license:gpl3+))))

(define-public guile-dmenu-development-manifest
  (concatenate-manifests
   (list (package->development-manifest (next guile-dmenu-devel))
         (packages->manifest
          (list guile-3.0-latest
                guile-ares-rs
                guile-bytestructure-class-next)))))

(define-public guile-dmenu-integration-manifest
  ;; The integration suite consumes guile-river's test helpers from its
  ;; controller-provided source tree.  It needs the compositor and input tool,
  ;; but not guile-river's benchmark-only development manifest.
  (concatenate-manifests
   (list guile-dmenu-development-manifest
         (packages->manifest (list pkg-config river-latest wtype)))))

;; `guix build -f' and `guix shell -f' identify their operation in the file's
;; command line.  Loading this module with `-e', however, supplies no second
;; argument.  Keep that supported so test drivers can request the development
;; manifest without depending on ambient Guile modules.
(match (cdr (command-line))
  (("build" . _) (next guile-dmenu-devel))
  (("shell" . _) guile-dmenu-development-manifest)
  (_ guile-dmenu-devel))
