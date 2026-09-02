; SPDX-License-Identifier: GPL-3.0-or-later

;; guile-river's test manager normally exits after the compositor's first
;; empty manage/render cycle.  An integration client starts later, after the
;; Wayland socket and manager readiness have been observed, so keep the
;; manager's dispatch loop active until River terminates the test session.
(let ((manager-source
       (or (getenv "GUILE_RIVER_TEST_MANAGER")
           (error "GUILE_RIVER_TEST_MANAGER is not set"))))
  (primitive-load manager-source))

(let* ((manager-module (resolve-module '(river test-manager)))
       (manage! (module-ref manager-module 'manage!)))
  (module-set!
   manager-module 'manage!
   (lambda (manager)
     (manage! manager)
     (module-set! manager-module 'manage-complete? #f))))

((module-ref (resolve-module '(river test-manager)) 'main) (command-line))
