;;; Integration environment for guile-dmenu.
;;; SPDX-License-Identifier: GPL-3.0-or-later

(primitive-load
 (string-append (or (getenv "GUILE_DMENU_PROJECT_DIR")
                    (error "GUILE_DMENU_PROJECT_DIR is not set"))
                "/.guix/modules/guile-dmenu-package.scm"))

(@@ (guile-dmenu-package) guile-dmenu-integration-manifest)
