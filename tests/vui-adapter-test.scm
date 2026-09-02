(use-modules (guile-dmenu filter)
             (guile-dmenu vui-adapter)
             (srfi srfi-64)
             (vui element))

(test-begin "vui-adapter")

(define state (make-completing-read-state "a" 1 '("alpha" "beta") 1))
(define model (completion-state->vui-model state))
(define tree (completion-model->vui-tree model))

(test-equal "model is an exact state snapshot"
  '("a" 1 1 ("alpha" "beta"))
  (map (lambda (key) (assq-ref model key))
       '(input-text cursor-position selected-index filtered-options)))
(string-set! (completing-read-state-input-text state) 0 #\z)
(test-equal "snapshot does not alias mutable state data" "a"
  (assq-ref model 'input-text))

(test-equal "tree has stable completion root" '(column completion)
  (list (element-kind tree) (element-key tree)))
(test-equal "tree contains input followed by every candidate"
  '(text-input button button)
  (map element-kind (element-children tree)))
(test-equal "text input uses the symbolic replace-input handler"
  'replace-input
  (assq-ref (element-properties (car (element-children tree))) 'on-change))
(test-equal "only selected candidate is actionable" '(#t #f)
  (map (lambda (node) (assq-ref (element-properties node) 'disabled?))
       (cdr (element-children tree))))

(test-equal "actions map to existing events"
  '(select next previous complete cancel
           (replace-input "xy" 1) (input-char #\x) #f)
  (map vui-action->completion-event
       '(accept move-next move-previous complete-next escape
                (replace-input "xy" 1) (input-char #\x) unknown)))

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
