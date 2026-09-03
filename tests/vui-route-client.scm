;;; Private-compositor exercise for guile-dmenu's VUI keyboard boundary.
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (guile-dmenu filter)
             (guile-dmenu vui-adapter)
             (ice-9 match)
             (srfi srfi-1)
             (vui guile-wayland))

(define scenario
  (match (command-line)
    ((_ value) (string->symbol value))
    (_ (error "expected details or comment scenario"))))
(define options '("alpha" "beta"))
(define (field model key) (assq-ref model key))
(define (with model key value) (acons key value (alist-delete key model)))
(define initial
  `((state . ,(make-completing-read-state "" 0 options))
    (mode . menu) (comment-state . #f) (answer . #f)))

(define (update model action)
  (let ((mode (field model 'mode))
        (state (field model 'state)))
    (match action
      ('complete
       (cond ((not (eq? mode 'menu)) model)
             ((eq? scenario 'details) (with model 'mode 'details))
             (else
              (with (with model 'mode 'comment) 'comment-state
                    (make-completing-read-state "" 0 '())))))
      ('cancel
       (if (eq? mode 'comment)
           (with (with model 'mode 'menu) 'comment-state #f)
           (with model 'answer '(cancelled))))
      ('select
       (cond ((eq? mode 'comment)
              (let ((text (completing-read-state-input-text
                           (field model 'comment-state))))
                (if (string-null? text) model
                    (with model 'answer
                          (list 'comment
                                (completing-read-state-selected-index state)
                                text)))))
             ((eq? mode 'details)
              (with model 'answer
                    (list 'details
                          (list-ref options
                                    (completing-read-state-selected-index state)))))
             (else model)))
      (_
       (let* ((comment? (eq? mode 'comment))
              (current (if comment? (field model 'comment-state) state))
              (result (dispatch-completion-vui-action
                       current action (if comment? '() options)
                       (if comment? '() options)
                       #:selection-mode (if comment? 'text 'menu)
                       #:input-enabled? comment?)))
         (if (eq? (car result) 'state-update)
             (with model (if comment? 'comment-state 'state) (cadr result))
             model))))))

(define (view model)
  (completion-state->vui-tree
   (field model 'state) #:prompt "D2.2" #:maximum 2
   #:option-details '("first detail" "second detail")
   #:details-visible? (eq? (field model 'mode) 'details)
   #:input-enabled? #f
   #:comment-state (field model 'comment-state)
   #:comment-index (and (field model 'comment-state)
                        (completing-read-state-selected-index
                         (field model 'state)))))

(define answer
  (run-wayland-vui
   initial update view #:title "guile-dmenu D2.2 route fixture"
   #:app-id "org.guile-dmenu.d22-route-fixture"
   #:logical-size (lambda (_) '(640 180)) #:route-key route-key
   #:terminal? (lambda (model) (and (field model 'answer) #t))
   #:result (lambda (model) (field model 'answer))))
(write answer)
(newline)
