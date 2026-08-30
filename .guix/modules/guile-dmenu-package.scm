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
  #:use-module (git))

(define-public guile-bytestructure-class-next
  ((package-input-rewriting/spec
    `(("guile" . ,(const guile-next))))
   (package/inherit guile-bytestructure-class
     (source
      (origin
        (inherit (package-source guile-bytestructure-class))
        (patches
         (list
          (origin
            (method url-fetch)
            (uri
             (string-append "https://github.com/Z572/guile-bytestructure-class"
                            "/commit/"
                            "87a2c7cd02305e4020be8226988128eff81d6ae4.patch"))
            (sha256
             (base32 "17mp7243djdslhimk3dps9svsjy5n2m28h9w9abjl2bp9z15xnak")))))))
     (arguments
      ;; XXX: guile-next breaks one test.
      (list #:tests? #f
            #:make-flags #~'("GUILE_AUTO_COMPILE=0"))))))

(define-public guile-wayland-next
  (package/inherit guile-wayland
    (source
     (origin
       (inherit (package-source guile-wayland))
       (patches
        (list
         (origin
           (method url-fetch)
           (uri
            (string-append "https://github.com/guile-wayland/guile-wayland"
                           "/commit/"
                           "cbc3e344a85fc243734d68d20a4013b55ed9596b.patch"))
           (sha256
            (base32 "1wlbkb6xphjlj7br5rzjl2cjjs5gky7fh224fm94gncgqdg3d1pc")))
         (origin
           (method url-fetch)
           (uri
            (string-append "https://github.com/guile-wayland/guile-wayland"
                           "/commit/"
                           "df85dee5d4bb5d55685944ab44033c172fc582ec.patch"))
           (sha256
            (base32 "1nfzjmf2whv4gbvialg5zrbks2am9dr1awkjn6yk2ny6awp35kx9")))
         (local-file
          (string-append (dirname (current-filename))
                         "/../patches/guile-wayland-client-event-object-type.patch"))))))))

(define next
  (package-input-rewriting/spec
   `(("guile" . ,(const guile-next))
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
                       (questions (string-append bin "guile-dmenu-questions")))
                  (install-file "scripts/dmenu" bin)
                  (install-file "scripts/codex-dmenu-approval" bin)
                  (install-file "scripts/guile-dmenu-questions" bin)
                  (for-each
                   (lambda (program)
                     (wrap-program program
                       #:sh (search-input-file inputs "bin/bash")
                       `("GUILE_AUTO_COMPILE" ":" = ("0"))
                       `("GUILE_LOAD_PATH" ":" prefix
                         ,(string-split (getenv "GUILE_LOAD_PATH") #\:))
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                         ,(string-split (getenv "GUILE_LOAD_COMPILED_PATH") #\:))))
                   (list dmenu approval questions))))))))
      (native-inputs (list guile-3.0))
      (inputs (list bash-minimal guile-3.0))
      (propagated-inputs (list guile-cairo
                               guile-fibers
                               guile-json-4
                               guile-wayland
                               guile-xkbcommon))
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
                   (list guile-next
                         guile-ares-rs
                         guile-bytestructure-class-next))))))
