(use-modules (guile-dmenu filter)
             (guile-dmenu vui-adapter)
             (srfi srfi-1)
             (srfi srfi-64)
             (vui element)
             (vui input)
             (vui layout)
             (vui runtime)
             (vui style))

(test-begin "vui-adapter")

(define state (make-completing-read-state "a" 1 '("alpha" "beta") 1))
(define model (completion-state->vui-model state))
(define tree (completion-model->vui-tree model))
(define input-row (car (element-children tree)))
(define candidates (car (filter (lambda (node)
                                  (eq? (element-key node) 'candidates))
                                (element-children tree))))

(test-equal "model is an exact state snapshot"
  '("a" 1 1 ("alpha" "beta"))
  (map (lambda (key) (assq-ref model key))
       '(input-text cursor-position selected-index filtered-options)))
(string-set! (completing-read-state-input-text state) 0 #\z)
(test-equal "snapshot does not alias mutable state data" "a"
  (assq-ref model 'input-text))

(test-equal "tree has stable completion root" '(column completion)
  (list (element-kind tree) (element-key tree)))
(test-equal "tree contains prompt/input row and candidate list"
  '(row list)
  (map element-kind (element-children tree)))
(test-equal "prompt and Unicode input are retained exactly"
  '("" "a")
  (map (lambda (node)
         (assq-ref (element-properties node)
                   (if (eq? (element-kind node) 'text) 'text 'value)))
       (element-children input-row)))
(test-equal "text input uses the symbolic replace-input handler"
  'replace-input
  (assq-ref (element-properties (cadr (element-children input-row))) 'on-change))
(test-equal "only selected candidate is actionable" '(#t #f)
  (map (lambda (node) (assq-ref (element-properties node) 'disabled?))
       (element-children candidates)))
(test-equal "selected row owns explicit highlight style"
  '(normal highlight)
  (map (lambda (node) (style-ref (element-style node) 'background-color))
       (element-children candidates)))

(let* ((paged-state (make-completing-read-state
                     "λ" 7 '("零" "一" "二" "三" "四" "五" "六" "七")))
       (paged (completion-state->vui-model
               paged-state #:prompt "選択 ›" #:maximum 3
               #:width 512 #:padding 7)))
  (test-equal "paging retains absolute selection and fills final page"
    '(5 7 2 ("五" "六" "七"))
    (map (lambda (key) (assq-ref paged key))
         '(visible-start selected-index visible-selected-index visible-options)))
  (let* ((paged-tree (completion-model->vui-tree paged))
         (list-node (car (filter (lambda (node)
                                  (eq? (element-key node) 'candidates))
                                (element-children paged-tree)))))
    (test-equal "paged row keys remain absolute indices" '(5 6 7)
      (map element-key (element-children list-node)))
    (test-equal "resize and padding are deterministic style inputs" '(512 7)
      (map (lambda (key) (style-ref (element-style paged-tree) key))
           '(width padding)))))

(let* ((empty (make-completing-read-state "∅" 0 '()))
       (empty-model (completion-state->vui-model
                     empty #:maximum 4 #:row-height 31 #:fixed-height? #t))
       (empty-tree (completion-model->vui-tree empty-model))
       (empty-candidates (car (filter (lambda (node)
                                        (eq? (element-key node) 'candidates))
                                      (element-children empty-tree)))))
  (test-equal "empty fixed-height menu reserves configured rows" '(0 4 ())
    (map (lambda (key) (assq-ref empty-model key))
         '(visible-start reserved-option-rows visible-options)))
  (test-equal "reserved empty rows are deterministic spacers" 4
    (count (lambda (node) (eq? (element-kind node) 'spacer))
           (element-children empty-candidates)))
  (test-equal "fixed-height spacers reserve actual candidate-row height"
    '(31 31 31 31)
    (map (lambda (node) (assq-ref (element-properties node) 'height))
         (element-children empty-candidates))))

(let* ((short-state (make-completing-read-state "" 0 '("one" "two")))
       (short-model (completion-state->vui-model
                     short-state #:maximum 4 #:row-height 29 #:fixed-height? #t))
       (short-tree (completion-model->vui-tree short-model))
       (short-candidates (car (filter (lambda (node)
                                        (eq? (element-key node) 'candidates))
                                      (element-children short-tree)))))
  (test-equal "fixed-height list contains candidates and reserved rows"
    '(button button spacer spacer)
    (map element-kind (element-children short-candidates)))
  (test-equal "candidate and reserved rows share configured height"
    '(29 29 29 29)
    (map (lambda (node)
           (if (eq? (element-kind node) 'spacer)
               (assq-ref (element-properties node) 'height)
               (style-ref (element-style node) 'height)))
         (element-children short-candidates)))
  (test-equal "fixed-height candidate list has maximum-times-row geometry" 116
    (rect-height
     (layout-rect
      (layout-tree short-candidates
                   #:measure (lambda (element maximum-width maximum-height)
                               (values 10 7)))))))

(let* ((zero-state (make-completing-read-state "" 0 '("one")))
       (zero-model (completion-state->vui-model
                    zero-state #:maximum 2 #:row-height 0 #:fixed-height? #t))
       (zero-tree (completion-model->vui-tree zero-model))
       (zero-candidates (car (filter (lambda (node)
                                       (eq? (element-key node) 'candidates))
                                     (element-children zero-tree)))))
  (test-equal "zero row height is accepted and retained"
    '(0 0)
    (list (assq-ref zero-model 'row-height)
          (style-ref (element-style (car (element-children zero-candidates)))
                     'height))))

(let* ((details-state (make-completing-read-state "" 1 '("Fast" "Safe")))
       (details (completion-state->vui-model
                 details-state #:message-lines '("Choose")
                 #:option-details '("quick" "careful")
                 #:details-visible? #t)))
  (test-equal "details mode appends the absolute selected row detail"
    '(details ("Choose" "careful"))
    (map (lambda (key) (assq-ref details key)) '(mode message-lines))))

(let* ((menu-state (make-completing-read-state "" 1 '("Fast" "Safe")))
       (editor (make-completing-read-state "理由" 2 '()))
       (comment (completion-state->vui-model
                 menu-state #:prompt "Mode?" #:message-lines '("keys" "context")
                 #:comment-state editor #:comment-index 1)))
  (test-equal "comment mode replaces prompt/input while preserving selection"
    '(comment "Comment ›" "理由" 2 1 1 #t)
    (map (lambda (key) (assq-ref comment key))
         '(mode prompt input-text cursor-position selected-index comment-index
                input-enabled?)))
  (test-equal "comment mode replaces only the shortcut message"
    '("Comment on selected choice  ·  Enter attach  ·  Esc back" "context")
    (assq-ref comment 'message-lines)))

(let* ((confirm-state (make-completing-read-state
                       "yes" 0 '("yes") 3 "yes"))
       (confirm (completion-state->vui-model confirm-state)))
  (test-equal "confirmation mode has a deterministic visible instruction"
    '("Confirm submission with RET") (assq-ref confirm 'message-lines)))

(test-equal "actions map to existing events"
  '(select next previous complete cancel
           (replace-input "xy" 1) (input-char #\x) #f)
  (map vui-action->completion-event
       '(accept move-next move-previous complete-next escape
                (replace-input "xy" 1) (input-char #\x) unknown)))

(test-equal "normalized VUI keys preserve the complete legacy key contract"
  '(cancel cancel cancel select select complete complete-previous
           complete-previous left right home end next previous next previous
           next-default previous-default backspace ctrl+backspace
           (input-text "界") #f #f (input-text "ab"))
  (map (lambda (input) (apply route-key input))
       '((Escape "" ()) (c "c" (control)) (g "g" (control))
         (Return "" ()) (KP_Enter "" ()) (Tab "" ())
         (Tab "" (shift)) (ISO_Left_Tab "" (shift))
         (Left "" ()) (Right "" ()) (Home "" ()) (End "" ())
         (Down "" ()) (Up "" ()) (n "n" (control)) (p "p" (control))
         (n "n" (alt)) (p "p" (alt)) (BackSpace "" ())
         (BackSpace "" (control)) (u "界" ()) (u "u" (alt))
         (F1 "" ()) (compose "ab" ()))))

(define (controller-press+repeat initial decoded options selection-mode)
  "Drive one physical press and its scheduled repeat through VUI's controller."
  (let ((callback #f) (actions '()) (outcomes '()))
    (define (update current action)
      (set! actions (append actions (list action)))
      (let ((outcome (dispatch-completion-vui-action
                      current action options options
                      #:selection-mode selection-mode)))
        (set! outcomes (append outcomes (list outcome)))
        (if (eq? (car outcome) 'state-update) (cadr outcome) current)))
    (define app
      (make-vui-app initial update
                    (lambda (current)
                      (completion-state->vui-tree current #:maximum 2))))
    (define input
      (make-input-controller
       app
       `((keymap-new . ,identity)
         (state-new . ,identity)
         (decode-key . ,(lambda (_state _keycode _pressed?) decoded))
         (schedule-repeat . ,(lambda (repeat)
                               (set! callback repeat)
                               'repeat-token))
         (cancel-repeat . ,(lambda (_token) #t)))
       (lambda () (layout-tree (vui-view app) #:width 320 #:height 200))
       #:route-key route-key))
    (input-keymap! input 'fixture-keymap)
    (input-key! input 9 #t)
    (unless (procedure? callback) (error "repeat was not scheduled" decoded))
    (callback)
    (input-key! input 9 #f)
    (input-close! input)
    (list actions outcomes (vui-model app))))

(define (state-summary state)
  (list (completing-read-state-input-text state)
        (completing-read-state-cursor-position state)
        (completing-read-state-selected-index state)
        (completing-read-state-completion-invoked? state)))

(let* ((options '("alpha" "alpine" "beta" "delta"))
       (cases
        `((editing ,(make-completing-read-state "" 0 options) (x "x" () #t) text)
          (backspace ,(make-completing-read-state "abcd" 0 options 4)
                     (BackSpace "" () #t) text)
          (cursor ,(make-completing-read-state "abcd" 0 options 3)
                  (Left "" () #t) text)
          (completion ,(make-completing-read-state "al" 0 options 2)
                      (Tab "" () #t) text)
          (paging ,(make-completing-read-state "" 0 options)
                  (Down "" () #t) menu)
          (accept ,(make-completing-read-state "" 0 options)
                  (Return "" () #t) menu)
          (cancel ,(make-completing-read-state "" 0 options)
                  (Escape "" () #t) menu)))
       (results
        (map (lambda (case)
               (controller-press+repeat (list-ref case 1) (list-ref case 2)
                                        options (list-ref case 3)))
             cases)))
  (test-equal "VUI press and repeat route the same complete action classes"
    '(((input-text "x") (input-text "x"))
      (backspace backspace) (left left) (complete complete)
      (next next) (select select) (cancel cancel))
    (map car results))
  (test-equal "repeated editing, cursor, completion, and paging stay domain-owned"
    '(("xx" 2 0 #f) ("ab" 2 0 #f) ("abcd" 1 0 #f)
      ("alp" 3 0 #t) ("" 0 2 #f))
    (map (lambda (result) (state-summary (list-ref result 2)))
         (take results 5)))
  (test-equal "repeated accept and cancel preserve terminal outcomes"
    '(((selected "alpha") (selected "alpha"))
      ((cancelled) (cancelled)))
    (map (lambda (result) (list-ref result 1)) (drop results 5))))

(test-error "route-key rejects malformed normalized input" #t
  (route-key 'a "a" '(control 1)))

(let* ((initial (make-completing-read-state "ac" 0 '("abc" "ac") 1))
       (inserted (dispatch-completion-vui-action
                  initial (route-key 'compose "界β" '())
                  '("abc" "ac") '("abc" "ac")))
       (updated (cadr inserted)))
  (test-equal "normalized compose text edits at the domain-owned cursor"
    '(state-update "a界βc" 3 ())
    (list (car inserted)
          (completing-read-state-input-text updated)
          (completing-read-state-cursor-position updated)
          (completing-read-state-filtered-options updated))))

(let* ((options '("alpha" "alpine"))
       (initial (make-completing-read-state "al" 0 options 2))
       (completed (dispatch-completion-vui-action
                   initial (route-key 'Tab "" '()) options options))
       (moved (dispatch-completion-vui-action
               (cadr completed) (route-key 'Left "" '()) options options)))
  (test-equal "completion and cursor movement remain domain transitions"
    '(state-update "alp" 2 #f)
    (list (car completed)
          (completing-read-state-input-text (cadr completed))
          (completing-read-state-cursor-position (cadr moved))
          (completing-read-state-completion-invoked? (cadr moved)))))

(test-equal "Tab routes details and comment workflows for their controller"
  '(complete complete complete-previous)
  (list (route-key 'Tab "" '())
        (route-key 'Tab "" '())
        (route-key 'ISO_Left_Tab "" '(shift))))

(test-equal "modified text is consumed except Shift-produced text"
  '(#f #f (input-text "A"))
  (list (route-key 'x "x" '(control shift))
        (route-key 'x "x" '(alt shift))
        (route-key 'A "A" '(shift))))

(let ((result (dispatch-completion-vui-action
               (make-completing-read-state "a" 0 '("alpha" "beta"))
               '(replace-input "be" 1) '("alpha" "beta") '("alpha" "beta"))))
  (test-equal "replace-input delegates filtering and cursor state"
    '(state-update "be" 1 ("beta"))
    (let ((updated (cadr result)))
      (list (car result)
            (completing-read-state-input-text updated)
            (completing-read-state-cursor-position updated)
            (completing-read-state-filtered-options updated)))))

(define fresh (make-completing-read-state "" 0 '("a" "b")))
(test-equal "navigation is delegated to domain transition" 1
  (completing-read-state-selected-index
   (cadr (dispatch-completion-vui-action fresh 'next '("a" "b") '("a" "b")
                                         #:selection-mode 'menu))))
(test-equal "acceptance preserves domain result" '(selected "a")
  (dispatch-completion-vui-action fresh 'accept '("a" "b") '("a" "b")
                                  #:selection-mode 'menu))
(test-equal "cancellation remains terminal and distinct" '(cancelled)
  (dispatch-completion-vui-action fresh 'cancel '("a" "b") '("a" "b")))
(test-equal "unknown actions are deterministic no-ops" 'no-change
  (car (dispatch-completion-vui-action fresh 'unknown '("a" "b") '("a" "b"))))

(let ((runner (test-runner-current)))
  (test-end "vui-adapter")
  (primitive-exit (if (zero? (test-runner-fail-count runner)) 0 1)))
