(use-modules (guile-dmenu completion)
             (guile-dmenu cursor-render)
             (guile-dmenu filter)
             (cairo)
             (ice-9 hash-table)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-64))

(define runner (test-runner-simple))
(test-runner-current runner)
(test-begin "submission transitions")

(define highlighted-state
  (make-completing-read-state "literal input" 1
                              '("first row" "highlighted row")))

(test-equal "submission defaults to literal text, not highlighted row"
  '(selected "literal input")
  (handle-submit highlighted-state))

(test-equal "text mode submits empty input"
  '(selected "")
  (handle-submit (make-completing-read-state "" 0 '("highlighted row"))
                 'text))

(test-equal "text mode submits input when there are no candidates"
  '(selected "not a candidate")
  (handle-submit (make-completing-read-state "not a candidate" 0 '())
                 'text))

(test-equal "menu mode returns highlighted row"
  '(selected "highlighted row")
  (handle-submit highlighted-state 'menu))

(test-equal "menu-index mode returns highlighted position"
  '(selected 1)
  (handle-submit highlighted-state 'menu-index))

(test-equal "menu mode preserves no-selection transition"
  'no-change
  (car (handle-submit (make-completing-read-state "literal input" 0 '())
                      'menu)))

(test-equal "menu-index mode preserves no-selection transition"
  'no-change
  (car (handle-submit (make-completing-read-state "literal input" 0 '())
                      'menu-index)))

(test-error "unsupported selection mode is rejected" #t
  (handle-submit highlighted-state 'invalid))

(test-end "submission transitions")

(test-begin "require-match submission")

(define require-match-options '("alpha" "beta"))

(test-equal "require-match #f accepts literal non-match"
  '(accepted "literal")
  (completion-submit "literal" require-match-options #f))

(test-equal "require-match #t accepts an exact match"
  '(accepted "alpha")
  (completion-submit "alpha" require-match-options #t))

(test-equal "require-match #t rejects a non-match"
  '(rejected "literal")
  (completion-submit "literal" require-match-options #t))

(test-equal "other truthy require-match rejects a non-match"
  '(rejected "literal")
  (completion-submit "literal" require-match-options 'strict))

(test-equal "confirm requests a second submission for a non-match"
  '(confirm "literal")
  (completion-submit "literal" require-match-options 'confirm))

(test-equal "confirm accepts the documented second submission"
  '(accepted "literal")
  (completion-submit "literal" require-match-options 'confirm
                     #:confirmation-input "literal"))

(test-equal "confirm does not accept a changed input as confirmation"
  '(confirm "changed")
  (completion-submit "changed" require-match-options 'confirm
                     #:confirmation-input "literal"))

(test-equal "confirm-after-completion normally accepts a non-match"
  '(accepted "literal")
  (completion-submit "literal" require-match-options
                     'confirm-after-completion))

(test-equal "confirm-after-completion requests confirmation after TAB"
  '(confirm "literal")
  (completion-submit "literal" require-match-options
                     'confirm-after-completion
                     #:completion-invoked? #t))

(test-equal "confirm-after-completion accepts the second submission"
  '(accepted "literal")
  (completion-submit "literal" require-match-options
                     'confirm-after-completion
                     #:confirmation-input "literal"))

(let ((seen '()))
  (define (accept-only-ok input)
    (set! seen (cons input seen))
    (string=? input "ok"))
  (test-equal "procedure require-match accepts when it returns true"
    '(accepted "ok")
    (completion-submit "ok" require-match-options accept-only-ok))
  (test-equal "procedure require-match rejects even an exact candidate"
    '(rejected "alpha")
    (completion-submit "alpha" require-match-options accept-only-ok))
  (test-equal "procedure require-match receives every submitted input"
    '("alpha" "ok") seen))

(define (submit-state input)
  (make-completing-read-state input 0
                              (filter-options require-match-options input)))

(let* ((first (handle-submit (submit-state "literal") 'text
                             #:collection require-match-options
                             #:require-match 'confirm))
       (pending (cadr first)))
  (test-equal "confirm first RET keeps the session open"
    'state-update (car first))
  (test-equal "confirm state retains the submitted input"
    "literal" (completing-read-state-confirmation-input pending))
  (test-equal "confirm second RET selects the literal input"
    '(selected "literal")
    (handle-submit pending 'text
                   #:collection require-match-options
                   #:require-match 'confirm)))

(let* ((completed (cadr (handle-complete
                         (submit-state "literal")
                         require-match-options require-match-options)))
       (first (handle-submit completed 'text
                             #:collection require-match-options
                             #:require-match 'confirm-after-completion))
       (pending (cadr first)))
  (test-assert "TAB invocation is represented in pure state"
    (completing-read-state-completion-invoked? completed))
  (test-equal "confirm-after-completion first RET keeps session open"
    'state-update (car first))
  (test-equal "confirm-after-completion second RET selects input"
    '(selected "literal")
    (handle-submit pending 'text
                   #:collection require-match-options
                   #:require-match 'confirm-after-completion)))

(let* ((completed (cadr (dispatch-completing-read-event
                         (submit-state "literal")
                         'complete require-match-options require-match-options)))
       (boundary-result
        (dispatch-completing-read-event
         completed 'right require-match-options require-match-options)))
  (test-equal "boundary no-op remains a no-change editing transition"
    'no-change (car boundary-result))
  (test-assert "a key between TAB and RET clears completion immediacy"
    (not (completing-read-state-completion-invoked? (cadr boundary-result))))
  (test-equal "RET after an intervening boundary key does not confirm"
    '(selected "literal")
    (dispatch-completing-read-event
     (cadr boundary-result) 'select require-match-options require-match-options
     #:require-match 'confirm-after-completion)))

(test-end "require-match submission")

(test-begin "collection normalization")

(define (displays candidates)
  (map completion-candidate-display candidates))

(test-equal "string list becomes display candidates"
  '("alpha" "beta")
  (displays (normalize-collection '("alpha" "beta"))))

(let* ((entries '(("one" . 1) ("two" . 2)))
       (candidates (normalize-collection entries)))
  (test-equal "alist keys become display candidates"
    '("one" "two")
    (displays candidates))
  (test-eq "alist candidate retains its original entry"
    (car entries)
    (completion-candidate-entry (car candidates))))

(test-equal "vector becomes display candidates"
  '("north" "south")
  (displays (normalize-collection #("north" "south"))))

(let ((table (make-hash-table)))
  (hash-set! table "left" 10)
  (hash-set! table "right" 20)
  (test-equal "hash keys become display candidates"
    '("left" "right")
    (sort (displays (normalize-collection table)) string<?)))

(let ((seen '()))
  (test-equal "predicate filters using complete alist entries"
    '("keep")
    (displays
     (normalize-collection
      '(("drop" . 1) ("keep" . 2))
      (lambda (entry)
        (set! seen (cons entry seen))
        (even? (cdr entry))))))
  (test-equal "predicate inspected the values"
    '(1 2)
    (sort (map cdr seen) <)))

(let* ((predicate (lambda (entry) #t))
       (action '(boundaries . "suffix"))
       (received #f)
       (table (lambda (input supplied-predicate supplied-action)
                (set! received
                      (list input supplied-predicate supplied-action))
                '("programmed"))))
  (test-equal "programmed result becomes display candidates"
    '("programmed")
    (displays (normalize-collection table predicate "pro" action)))
  (test-equal "programmed table receives input, predicate, and action"
    (list "pro" predicate action)
    received))

(let ((predicate (lambda (entry) #t)))
  (for-each
   (lambda (action)
     (test-equal (string-append "programmed action is forwarded: "
                                (object->string action))
       (list "input" predicate action)
       (call-completion-table
        (lambda args args) "input" predicate action)))
   (list #f #t 'lambda 'metadata '(boundaries . "suffix"))))

(let* ((predicate (lambda (entry) #t))
       (calls '())
       (table
        (lambda (input supplied-predicate action)
          (set! calls (cons (list input supplied-predicate action) calls))
          (cond
           ((eq? action 'metadata)
            '(metadata (category . file)))
           ((and (pair? action) (eq? (car action) 'boundaries))
            '(boundaries 4 . 1))
           (else #f)))))
  (test-equal "programmed metadata is normalized"
    '(metadata (category . file))
    (completion-metadata "dir/na" table predicate))
  (test-equal "programmed boundaries are normalized"
    '(4 . 1)
    (completion-boundaries "dir/na" table predicate "me"))
  (test-equal "metadata and boundaries preserve the predicate object"
    (list predicate predicate)
    (map cadr (reverse calls)))
  (test-equal "boundary action contains the suffix"
    '(boundaries . "me")
    (caddr (car calls))))

(test-equal "concrete collections have empty metadata"
  '(metadata)
  (completion-metadata "a" '("alpha")))

(test-equal "concrete boundaries cover the complete input field"
  '(0 . 2)
  (completion-boundaries "dir/na" '("alpha") #f "me"))

(test-equal "malformed programmed metadata falls back to empty metadata"
  '(metadata)
  (completion-metadata
   "a" (lambda (input predicate action) 'malformed)))

(test-equal "malformed programmed boundaries fall back to the full field"
  '(0 . 2)
  (completion-boundaries
   "a" (lambda (input predicate action) '(boundaries 9 . 9)) #f "bc"))

(test-end "collection normalization")

(test-begin "public initial collection input")

(define (install-interface name exports)
  (let ((module (define-module* name #:exports exports)))
    (for-each (lambda (symbol)
                (module-define! module symbol #f)
                (module-export! module (list symbol)))
              exports)))

;; The public procedure reaches programmed collections before touching these
;; compositor interfaces.  Stubs let this production-path regression load the
;; real menu module even when guile-wayland is not installed in the unit-test
;; environment.
(install-interface '(guile-dmenu wayland)
                   '(wayland-connection-surface wayland-connection-display
                     wayland-connection-shm connect-wayland create-window
                     wl-display-cancel-read wl-display-dispatch-pending
                     wl-display-prepare-read))
(install-interface '(guile-dmenu graphics)
                   '(draw-menu make-menu-buffer-cache
                     destroy-menu-buffer-cache! wrap-message-lines))
(install-interface '(guile-dmenu keyboard)
                   '(make-keyboard-session set-keyboard-session-key-handler!
                     make-key-decoder make-seat-listener))
(install-interface '(wayland client display)
                   '(wl-display-get-fd wl-display-flush wl-display-read-events
                     wl-display-disconnect))
(install-interface '(wayland client protocol wayland)
                   '(wl-surface-attach wl-surface-damage wl-surface-commit))
(install-interface '(wayland client protocol xdg-shell)
                   '(xdg-surface-ack-configure))

(let ((received #f))
  (let ((completing-read
         (module-ref (resolve-interface '(guile-dmenu menu))
                     'completing-read)))
    (catch 'initial-input-observed
      (lambda ()
        (completing-read
         "Prompt"
         (lambda (input predicate action)
           (set! received input)
           ;; Abort before completing-read attempts a Wayland connection.
           (throw 'initial-input-observed))
         #f #f '("seed text" . 4)))
      (lambda args #t)))
  (test-equal "completing-read sends normalized initial text to a programmed table"
    "seed text" received))

(test-end "public initial collection input")

(test-begin "in-memory cursor rendering")

(define render-width 240)
(define render-padding 4)
(define render-height 22)

(define (measured-cursor-x cr input input-x cursor-position)
  (cairo-select-font-face cr "Sans" 'normal 'normal)
  (cairo-set-font-size cr 14)
  (inexact->exact
   (floor (+ input-x 2
             (cairo-text-extents:x-advance
              (cairo-text-extents cr
                                  (substring input 0 cursor-position)))))))

(define (rendered-cursor-x input cursor-position)
  (let* ((surface (cairo-image-surface-create
                   'argb32 render-width render-height))
         (cr (cairo-create surface))
         (input-x 24)
         (expected (measured-cursor-x cr input input-x cursor-position)))
    ;; Black text on a black surface leaves the white cursor as the only
    ;; non-black pixels, so its rectangle can be located in the image data.
    (cairo-set-source-rgb cr 0 0 0)
    (cairo-paint cr)
    (draw-input-cursor! cr input cursor-position input-x render-padding
                        render-height '(1 1 1))
    (cairo-surface-flush surface)
    (let* ((pixels (cairo-image-surface-get-data surface))
           (stride (inexact->exact
                    (cairo-image-surface-get-stride surface)))
           (painted-columns
            (filter
             (lambda (x)
               (any (lambda (y)
                      (let ((offset (+ (* y stride) (* x 4))))
                        (or (> (bytevector-u8-ref pixels offset) 0)
                            (> (bytevector-u8-ref pixels (+ offset 1)) 0)
                            (> (bytevector-u8-ref pixels (+ offset 2)) 0))))
                    (iota render-height)))
             (iota render-width))))
      (cairo-destroy cr)
      (cairo-surface-destroy surface)
      (list expected (and (pair? painted-columns) (car painted-columns))))))

(for-each
 (lambda (fixture)
   (let ((label (car fixture))
         (input (cadr fixture))
         (position (caddr fixture)))
     (let ((coordinates (rendered-cursor-x input position)))
       (test-equal label (car coordinates) (cadr coordinates)))))
 '(("cursor rectangle is at the beginning of empty input" "" 0)
   ("cursor rectangle is in the middle of input" "abcd" 2)
   ("cursor rectangle is at trailing-space x-advance" "ab " 3)))

(let* ((surface (cairo-image-surface-create 'argb32 1 1))
       (cr (cairo-create surface))
       (extents (cairo-text-extents cr "ab ")))
  (test-assert "trailing whitespace advances beyond its ink rectangle"
    (> (cairo-text-extents:x-advance extents)
       (cairo-text-extents:width extents)))
  (cairo-destroy cr)
  (cairo-surface-destroy surface))

(test-end "in-memory cursor rendering")

(test-begin "substring completion")

(define completion-fixture
  '("Alpha" "alphabet" "Alpine" "beta"))

(test-equal "substring matching ignores case by default"
  '("Alpha" "alphabet")
  (completion-all-completions "PHA" completion-fixture))

(parameterize ((completion-ignore-case? #f))
  (test-equal "case-sensitive substring matching accepts matching case"
    '("Alpha")
    (completion-all-completions "Alph" completion-fixture))
  (test-equal "case-sensitive substring matching rejects different case"
    '()
    (completion-all-completions "PHA" completion-fixture)))

(test-equal "unique completion inserts the complete candidate"
  "beta"
  (completion-try-completion "eta" completion-fixture))

(test-equal "case-folded unique completion preserves canonical casing"
  "Alpha"
  (completion-try-completion "ALPHA" '("Alpha")))

(test-equal "ambiguous completion extends a shared prefix"
  "Alp"
  (completion-try-completion "al" completion-fixture))

(test-equal "ambiguous substring without a prefix extension stays unchanged"
  "pha"
  (completion-try-completion "pha" completion-fixture))

(test-assert "exact match is detected case-insensitively"
  (completion-exact-match? "ALPHA" completion-fixture))

(parameterize ((completion-ignore-case? #f))
  (test-assert "case-sensitive exact match rejects different case"
    (not (completion-exact-match? "ALPHA" completion-fixture))))

(test-equal "an already exact unique completion reports no insertion"
  #t
  (completion-try-completion "beta" completion-fixture))

(test-equal "no matches returns an empty all-completions result"
  '()
  (completion-all-completions "zzz" completion-fixture))

(test-equal "no matches cannot be completed"
  #f
  (completion-try-completion "zzz" completion-fixture))

(test-assert "no matches are not exact"
  (not (completion-exact-match? "zzz" completion-fixture)))

(test-assert "substring is the registered built-in style"
  (procedure? (lookup-completion-style 'substring)))

(let ((seen #f))
  (register-completion-style!
   'test-style
   (lambda (input candidates action)
     (set! seen (list input (map completion-candidate-display candidates)
                      action))
     'style-result))
  (test-equal "registered styles handle completion actions"
    'style-result
    (run-completion-action "x" '("one" "two") #t #f
                           #:style 'test-style))
  (test-equal "registered style receives normalized candidates"
    '("x" ("one" "two") #t)
    seen))

(let ((received #f))
  (test-equal "programmed tables own completion actions"
    'programmed-result
    (run-completion-action
     "needle"
     (lambda (input predicate action)
       (set! received (list input predicate action))
       'programmed-result)
     #f))
  (test-equal "try-completion action is forwarded to programmed table"
    '("needle" #f #f)
    received))

(test-end "substring completion")

(test-begin "TAB completion transitions")

(define tab-options '("alpha" "alpine" "beta"))

(define (tab-state input)
  (make-completing-read-state input 0 (filter-options tab-options input)))

(let ((result (handle-complete (tab-state "bet") tab-options tab-options)))
  (test-equal "TAB inserts a unique completion"
    'state-update
    (car result))
  (test-equal "unique TAB replaces input with the candidate"
    "beta"
    (completing-read-state-input-text (cadr result))))

(let ((result (handle-complete (tab-state "al") tab-options tab-options)))
  (test-equal "ambiguous TAB inserts only a longer shared prefix"
    "alp"
    (completing-read-state-input-text (cadr result)))
  (test-equal "ambiguous TAB retains all prefix-sharing candidates"
    '("alpha" "alpine")
    (completing-read-state-filtered-options (cadr result))))

(let* ((state (tab-state "zzz"))
       (result (handle-complete state tab-options tab-options)))
  (test-equal "no-match TAB records a state transition"
    'state-update
    (car result))
  (test-equal "no-match TAB leaves input unchanged"
    "zzz"
    (completing-read-state-input-text (cadr result)))
  (test-assert "no-match TAB records completion invocation"
    (completing-read-state-completion-invoked? (cadr result))))

(register-completion-style!
 'tab-test-style
 (lambda (input candidates action)
   (and (not action) "chosen-by-style")))

(let ((result (handle-complete (tab-state "a") tab-options tab-options
                               #:style 'tab-test-style)))
  (test-equal "TAB honors the selected completion style"
    "chosen-by-style"
    (completing-read-state-input-text (cadr result))))

(let* ((predicate (lambda (entry) #t))
       (actions '())
       (table
        (lambda (input supplied-predicate action)
          (set! actions
                (cons (list input supplied-predicate action) actions))
          (cond
           ((eq? action 'metadata)
            '(metadata (category . path)))
           ((and (pair? action) (eq? (car action) 'boundaries))
            '(boundaries 4 . 2))
           ((not action) "name")
           (else #f))))
       (state (make-completing-read-state "cmd nme tail" 0
                                          '("cmd name tail") 5))
       (updated (cadr (handle-complete state table
                                       '("cmd name tail") predicate))))
  (test-equal "TAB records programmed metadata in pure state"
    '(metadata (category . path))
    (completing-read-state-completion-metadata updated))
  (test-equal "TAB records programmed boundaries in pure state"
    '(4 . 2)
    (completing-read-state-completion-boundaries updated))
  (test-equal "TAB replaces only the programmed completion field"
    "cmd name tail"
    (completing-read-state-input-text updated))
  (test-equal "TAB places point immediately after the replacement"
    8
    (completing-read-state-cursor-position updated))
  (test-equal "TAB boundary query splits input at the cursor"
    '("cmd n" (boundaries . "me tail"))
    (let ((boundary-call
           (find (lambda (call)
                   (and (pair? (caddr call))
                        (eq? (car (caddr call)) 'boundaries)))
                 actions)))
      (list (car boundary-call) (caddr boundary-call))))
  (test-equal "TAB completes the field selected by the boundaries"
    "nme"
    (car (find (lambda (call) (not (caddr call))) actions)))
  (test-assert "TAB forwards the underlying-entry predicate for every action"
    (every (lambda (call) (eq? (cadr call) predicate)) actions)))

(test-end "TAB completion transitions")

(test-begin "cursor-aware editing transitions")

(define cursor-options '("abcde" "abXcde" "abde" "acde"))
(define middle-state
  (make-completing-read-state "abcde" 0 cursor-options 2))

(define (updated-state transition)
  (test-equal "transition updates state" 'state-update (car transition))
  (cadr transition))

(test-equal "Left moves cursor one character backward" 1
  (completing-read-state-cursor-position
   (updated-state (handle-left middle-state))))
(test-equal "Right moves cursor one character forward" 3
  (completing-read-state-cursor-position
   (updated-state (handle-right middle-state))))
(test-equal "Home moves cursor to the beginning" 0
  (completing-read-state-cursor-position
   (updated-state (handle-home middle-state))))
(test-equal "End moves cursor to the end" 5
  (completing-read-state-cursor-position
   (updated-state (handle-end middle-state))))

(let ((state (updated-state
              (handle-input-char #\X middle-state cursor-options))))
  (test-equal "input inserts at the cursor" "abXcde"
    (completing-read-state-input-text state))
  (test-equal "insertion advances the cursor" 3
    (completing-read-state-cursor-position state)))

(let ((state (updated-state (handle-backspace middle-state cursor-options))))
  (test-equal "backspace deletes before the cursor" "acde"
    (completing-read-state-input-text state))
  (test-equal "backspace retreats the cursor" 1
    (completing-read-state-cursor-position state)))

(let* ((state (make-completing-read-state "one two tail" 0 '() 7))
       (edited (updated-state
                (handle-backspace state '() #:ctrl-pressed? #t))))
  (test-equal "Ctrl+Backspace preserves text after the cursor" "one tail"
    (completing-read-state-input-text edited))
  (test-equal "Ctrl+Backspace moves to the deleted word boundary" 3
    (completing-read-state-cursor-position edited)))

(let* ((state (make-completing-read-state "   tail" 0 '() 3))
       (edited (updated-state
                (handle-backspace state '() #:ctrl-pressed? #t))))
  (test-equal "Ctrl+Backspace removes whitespace before the cursor" "tail"
    (completing-read-state-input-text edited))
  (test-equal "whitespace deletion moves the cursor to the beginning" 0
    (completing-read-state-cursor-position edited)))

(let ((start (make-completing-read-state "abc" 0 '("abc") 0))
      (finish (make-completing-read-state "abc" 0 '("abc") 3)))
  (test-equal "Left at start is a no-op" 'no-change
    (car (handle-left start)))
  (test-equal "Home at start is a no-op" 'no-change
    (car (handle-home start)))
  (test-equal "Right at end is a no-op" 'no-change
    (car (handle-right finish)))
  (test-equal "End at end is a no-op" 'no-change
    (car (handle-end finish)))
  (test-equal "Backspace at start is a no-op" 'no-change
    (car (handle-backspace start '("abc")))))

(test-error "cursor position cannot exceed the input" #t
  (make-completing-read-state "abc" 0 '() 4))

(test-end "cursor-aware editing transitions")

(test-begin "initial input")

(let ((state (initial-state '("alpha" "alphabet" "beta") "alp")))
  (test-equal "string initial input is installed" "alp"
    (completing-read-state-input-text state))
  (test-equal "string initial input places cursor at end" 3
    (completing-read-state-cursor-position state))
  (test-equal "string initial input filters candidates"
    '("alpha" "alphabet")
    (completing-read-state-filtered-options state)))

(let ((state (initial-state '("alpha" "beta") '("alpha" . 2))))
  (test-equal "pair initial input is installed" "alpha"
    (completing-read-state-input-text state))
  (test-equal "pair initial input uses its zero-based cursor position" 2
    (completing-read-state-cursor-position state)))

(test-error "negative initial cursor position is rejected" #t
  (initial-state '("alpha") '("alpha" . -1)))

(test-error "initial cursor past the end is rejected" #t
  (initial-state '("alpha") '("alpha" . 6)))

(test-error "non-integer initial cursor position is rejected" #t
  (initial-state '("alpha") '("alpha" . 1.5)))

(test-error "inexact initial cursor position is rejected" #t
  (initial-state '("alpha") '("alpha" . 1.0)))

(test-error "non-string initial input is rejected" #t
  (initial-state '("alpha") '(alpha . 1)))

(test-end "initial input")

(test-begin "defaults and input transformation")

(let ((state (initial-state '("one" "two") "" "fallback")))
  (test-equal "empty submission returns a scalar default"
    '(selected "fallback")
    (handle-submit state 'text #:collection '("one" "two"))))

(let ((state (initial-state '("one" "two") ""
                            '("first default" "second default"))))
  (test-equal "empty submission returns the first list default"
    '(selected "first default")
    (handle-submit state 'text #:collection '("one" "two"))))

(let ((state (initial-state '("literal") "" #f)))
  (test-equal "empty submission without a default remains empty"
    '(selected "")
    (handle-submit state 'text #:collection '("literal"))))

(let* ((options '("first default" "second default" "other"))
       (initial (initial-state options "typed"
                              '("first default" "second default")))
       (first (updated-state (handle-next-default initial options)))
       (second (updated-state (handle-next-default first options))))
  (test-equal "forward default navigation installs the first default"
    "first default" (completing-read-state-input-text first))
  (test-equal "default navigation places the cursor at the end"
    13 (completing-read-state-cursor-position first))
  (test-equal "default navigation refilters editable candidates"
    '("first default")
    (completing-read-state-filtered-options first))
  (test-equal "forward default navigation advances through list defaults"
    "second default" (completing-read-state-input-text second))
  (test-equal "backward default navigation restores the previous default"
    "first default"
    (completing-read-state-input-text
     (updated-state (handle-previous-default second options))))
  (test-equal "navigation past the last default is a no-op"
    'no-change (car (handle-next-default second options))))

(let ((calls 0))
  (parameterize ((completion-input-transformer
                  (lambda (input)
                    (set! calls (+ calls 1))
                    (string-upcase input))))
    (let* ((options '("a" "A"))
           (literal (updated-state
                     (handle-input-char #\a
                                        (initial-state options "" #f #f)
                                        options)))
           (inherited (updated-state
                       (handle-input-char #\a
                                          (initial-state options "" #f #t)
                                          options))))
      (test-equal "transformer is skipped without input-method inheritance"
        "a" (completing-read-state-input-text literal))
      (test-equal "transformer is applied with input-method inheritance"
        "A" (completing-read-state-input-text inherited))
      (test-equal "transformer is only invoked for inherited input" 1 calls))))

(parameterize ((completion-input-transformer
                (lambda (input) (string-append input input))))
  (let ((state (updated-state
                (handle-input-char #\x
                                   (initial-state '("xx") "" #f #t)
                                   '("xx")))))
    (test-equal "multi-character transformation is inserted" "xx"
      (completing-read-state-input-text state))
    (test-equal "cursor advances by transformed input length" 2
      (completing-read-state-cursor-position state))))

(test-error "defaults must be strings" #t
  (initial-state '("one") "" '("valid" 2)))

(test-end "defaults and input transformation")

(test-begin "completion history")

(let* ((history (make-completion-history '("newest" "older" "oldest")))
       (state (initial-state '("typed" "newest" "older" "oldest")
                             "typed" #f #f history)))
  (test-eq "history object is retained in pure state" history
    (completing-read-state-history state))
  (test-equal "unpositioned history starts before its newest entry" 0
    (completing-read-state-history-position state)))

(let* ((history (make-completion-history '("newest" "older" "oldest")))
       (state (initial-state '("newest" "older" "oldest")
                             "older" #f #f (cons history 2))))
  (test-equal "positioned history installs its one-based position" 2
    (completing-read-state-history-position state))
  (test-equal "positioned history keeps the matching initial input" "older"
    (completing-read-state-input-text state))
  (test-equal "previous from a positioned history selects the older entry"
    "oldest"
    (completing-read-state-input-text
     (updated-state (handle-previous-history
                     state '("newest" "older" "oldest")))))
  (test-equal "next from a positioned history selects the newer entry"
    "newest"
    (completing-read-state-input-text
     (updated-state (handle-next-history
                     state '("newest" "older" "oldest"))))))

(let* ((options '("typed" "newest" "older" "default one" "default two"))
       (history (make-completion-history '("newest" "older")))
       (start (initial-state options "typed"
                             '("default one" "default two") #f history))
       (newest (updated-state (handle-previous-history start options)))
       (older (updated-state (handle-previous-history newest options)))
       (back-newest (updated-state (handle-next-history older options)))
       (back-input (updated-state (handle-next-history back-newest options)))
       (first-default (updated-state (handle-next-history back-input options)))
       (second-default (updated-state
                        (handle-next-history first-default options)))
       (back-default (updated-state
                      (handle-previous-history second-default options)))
       (back-start (updated-state
                    (handle-previous-history back-default options))))
  (test-equal "M-p visits the newest history entry" "newest"
    (completing-read-state-input-text newest))
  (test-equal "repeated M-p visits older history" "older"
    (completing-read-state-input-text older))
  (test-equal "M-n walks back toward newer history" "newest"
    (completing-read-state-input-text back-newest))
  (test-equal "M-n restores the original editable input" "typed"
    (completing-read-state-input-text back-input))
  (test-equal "M-n exposes the first default as future history" "default one"
    (completing-read-state-input-text first-default))
  (test-equal "M-n advances through future defaults" "default two"
    (completing-read-state-input-text second-default))
  (test-equal "M-p walks backward through future defaults" "default one"
    (completing-read-state-input-text back-default))
  (test-equal "M-p from the first default restores initial input" "typed"
    (completing-read-state-input-text back-start)))

(let* ((history (make-completion-history '("old")))
       (state (initial-state '("accepted") "accepted" #f #f history)))
  (test-equal "successful text submission returns its input"
    '(selected "accepted")
    (handle-submit state 'text #:collection '("accepted")))
  (test-equal "successful text submission records newest first"
    '("accepted" "old")
    (completion-history-entries history))
  (handle-submit state 'text #:collection '("accepted"))
  (test-equal "consecutive history duplicates are suppressed"
    '("accepted" "old")
    (completion-history-entries history)))

(let ((history (make-completion-history '("old"))))
  (handle-submit
   (initial-state '("candidate") "literal" #f #f history)
   'text #:collection '("candidate") #:require-match #t)
  (test-equal "rejected text submission does not record history"
    '("old") (completion-history-entries history))
  (handle-submit
   (initial-state '("row") "typed" #f #f history)
   'menu #:collection '("row"))
  (test-equal "menu submission does not record completion history"
    '("old") (completion-history-entries history)))

(parameterize ((completion-history-length 2))
  (let ((history (make-completion-history '("second" "oldest"))))
    (completion-history-add! history "newest")
    (test-equal "history length drops oldest entries"
      '("newest" "second")
      (completion-history-entries history))))

(let ((history (make-completion-history '("one"))))
  (set-completion-history-entries! history '("replacement"))
  (test-equal "history entries expose intentional mutation"
    '("replacement") (completion-history-entries history)))

(test-error "positioned history requires a positive position" #t
  (initial-state '("one") "one" #f #f
                 (cons (make-completion-history '("one")) 0)))

(test-error "positioned history cannot exceed its entries" #t
  (initial-state '("one") "one" #f #f
                 (cons (make-completion-history '("one")) 2)))

(let ((state (initial-state '("one") "one" #f #f #t)))
  (test-assert "history #t explicitly disables recording"
    (not (completing-read-state-history state))))

(test-end "completion history")
(primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1))
