(define-module (guile-dmenu filter)
  #:use-module (guile-dmenu completion)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (filter-options
            handle-submit
            handle-select
            handle-next
            handle-previous
            handle-left
            handle-right
            handle-home
            handle-end
            handle-backspace
            handle-input-char
            handle-complete
            handle-next-history
            handle-previous-history
            handle-next-default
            handle-previous-default
            clear-completion-immediacy
            dispatch-completing-read-event
            wrap-navigation?
            ;; State record exports
            make-completing-read-state
            completing-read-state?
            completing-read-state-input-text
            completing-read-state-cursor-position
            completing-read-state-selected-index
            completing-read-state-filtered-options
            completing-read-state-confirmation-input
            completing-read-state-completion-invoked?
            completing-read-state-defaults
            completing-read-state-default-index
            completing-read-state-inherit-input-method?
            completing-read-state-history
            completing-read-state-history-position
            completing-read-state-history-start-position
            completing-read-state-history-start-input
            completing-read-state-completion-metadata
            completing-read-state-completion-boundaries
            normalize-defaults
            normalize-initial-input
            initial-state))

;; Define a record type to hold completing-read state
(define-record-type <completing-read-state>
  (%make-completing-read-state input-text selected-index filtered-options
                               cursor-position confirmation-input
                               completion-invoked? defaults default-index
                               inherit-input-method? history history-position
                               history-start-position history-start-input
                               completion-metadata completion-boundaries)
  completing-read-state?
  (input-text completing-read-state-input-text)
  (selected-index completing-read-state-selected-index)
  (filtered-options completing-read-state-filtered-options)
  (cursor-position completing-read-state-cursor-position)
  (confirmation-input completing-read-state-confirmation-input)
  (completion-invoked? completing-read-state-completion-invoked?)
  (defaults completing-read-state-defaults)
  (default-index completing-read-state-default-index)
  (inherit-input-method? completing-read-state-inherit-input-method?)
  (history completing-read-state-history)
  (history-position completing-read-state-history-position)
  (history-start-position completing-read-state-history-start-position)
  (history-start-input completing-read-state-history-start-input)
  (completion-metadata completing-read-state-completion-metadata)
  (completion-boundaries completing-read-state-completion-boundaries))

;; Keep the original three-argument constructor usable while making the
;; cursor explicit for callers that need to construct an editing state.
(define* (make-completing-read-state input-text selected-index filtered-options
                                     #:optional
                                     (cursor-position (string-length input-text))
                                     (confirmation-input #f)
                                     (completion-invoked? #f)
                                     (defaults '())
                                     (default-index -1)
                                     (inherit-input-method? #f)
                                     (history #f)
                                     (history-position 0)
                                     (history-start-position history-position)
                                     (history-start-input input-text)
                                     (completion-metadata '(metadata))
                                     (completion-boundaries '(0 . 0)))
  (unless (and (integer? cursor-position)
               (<= 0 cursor-position (string-length input-text)))
    (error "cursor position is outside input" cursor-position input-text))
  (%make-completing-read-state input-text selected-index filtered-options
                               cursor-position confirmation-input
                               completion-invoked? defaults default-index
                               inherit-input-method? history history-position
                               history-start-position history-start-input
                               completion-metadata completion-boundaries))

(define (normalize-defaults default)
  "Return DEFAULT as a validated list of strings."
  (let ((defaults (cond ((not default) '())
                        ((list? default) default)
                        (else (list default)))))
    (unless (every string? defaults)
      (error "completion default is not a string or list of strings" default))
    defaults))

(define (normalize-initial-input initial-input)
  "Return INITIAL-INPUT's text and zero-based cursor position.

INITIAL-INPUT may be a string, which places the cursor at its end, or a pair
whose car is a string and whose cdr is an integer position within that string."
  (let ((text (if (pair? initial-input) (car initial-input) initial-input))
        (position (if (pair? initial-input)
                      (cdr initial-input)
                      (and (string? initial-input)
                           (string-length initial-input)))))
    (unless (string? text)
      (error "initial input is not a string" initial-input))
    (unless (and (integer? position) (exact? position)
                 (<= 0 position (string-length text)))
      (error "initial input cursor position is outside input"
             position text))
    (values text position)))

;; Create initial state, filtering from the supplied initial text and placing
;; the cursor either at the end of a string or at a pair's explicit position.
(define* (initial-state options #:optional (initial-input "") (default #f)
                        (inherit-input-method? #f) (history-argument #f)
                        (initial-selected-index 0))
  (call-with-values
      (lambda () (normalize-initial-input initial-input))
    (lambda (text position)
      (call-with-values
          (lambda () (normalize-history history-argument))
        (lambda (history history-position)
          (let ((filtered (filter-options options text)))
            (unless (and (integer? initial-selected-index)
                         (exact? initial-selected-index)
                         (>= initial-selected-index 0)
                         (if (null? filtered)
                             (zero? initial-selected-index)
                             (< initial-selected-index (length filtered))))
              (error "initial selected index is outside options"
                     initial-selected-index))
            (make-completing-read-state
             text initial-selected-index filtered position #f #f
             (normalize-defaults default) -1 inherit-input-method?
             history history-position history-position text)))))))

;; When enabled, moving past either end of the list wraps to the other end
;; (bemenu's -w/--wrap).
(define wrap-navigation? (make-parameter #f))

;; Filter options based on input text
(define (filter-options options input)
  (if (string-null? input)
      options
      (filter
       (lambda (opt)
         (string-contains-ci opt input))
       options)))

;; A keyboard event other than TAB or RET breaks the adjacency required by
;; `confirm-after-completion'.  Keep this separate from individual editing
;; transitions so boundary no-ops clear the flag too.
(define (clear-completion-immediacy state)
  (if (not (completing-read-state-completion-invoked? state))
      state
      (make-completing-read-state
       (completing-read-state-input-text state)
       (completing-read-state-selected-index state)
       (completing-read-state-filtered-options state)
       (completing-read-state-cursor-position state)
       (completing-read-state-confirmation-input state) #f
       (completing-read-state-defaults state)
       (completing-read-state-default-index state)
       (completing-read-state-inherit-input-method? state)
       (completing-read-state-history state)
       (completing-read-state-history-position state)
       (completing-read-state-history-start-position state)
       (completing-read-state-history-start-input state)
       (completing-read-state-completion-metadata state)
       (completing-read-state-completion-boundaries state))))

;; Dispatch one event produced by `make-key-decoder'.  Keeping this transition
;; independent of the Wayland loop makes decoded boundary no-ops observable
;; while ensuring they still break TAB/RET adjacency in production.
(define* (dispatch-completing-read-event state event options collection
                                         #:key (predicate #f)
                                         (selection-mode 'text)
                                         (require-match #f)
                                         (style 'substring)
                                         (input-enabled? #t))
  (let ((state (if (or (memq event '(next previous next-default
                                      previous-default left right home end
                                      backspace ctrl+backspace))
                       (and (pair? event) (eq? (car event) 'input-char)))
                   (clear-completion-immediacy state)
                   state)))
    (match event
      ('select
       (handle-submit state selection-mode
                      #:collection collection
                      #:predicate predicate
                      #:require-match require-match
                      #:style style))
      ('next (handle-next state))
      ('previous (handle-previous state))
      ('next-default
       (if input-enabled?
           (handle-next-history state options)
           (list 'no-change state)))
      ('previous-default
       (if input-enabled?
           (handle-previous-history state options)
           (list 'no-change state)))
      ((and (or 'left 'right 'home 'end) key)
       (if input-enabled?
           ((case key
              ((left) handle-left)
              ((right) handle-right)
              ((home) handle-home)
              ((end) handle-end))
            state)
           (list 'no-change state)))
      ((and (or 'backspace 'ctrl+backspace) key)
       (if input-enabled?
           (handle-backspace state options
                             #:ctrl-pressed? (eq? key 'ctrl+backspace))
           (list 'no-change state)))
      ('complete
       (if input-enabled?
           (handle-complete state collection options predicate #:style style)
           (list 'no-change state)))
      ('complete-previous
       (list 'no-change state))
      (('input-char char)
       (if input-enabled?
           (handle-input-char char state options)
           (list 'no-change state)))
      (_ (list 'no-change state)))))

;; Helper to update state with new input text and reset selection
(define* (update-text-and-filter state new-text options
                                 #:optional (cursor-position
                                             (string-length new-text)))
  (let ((new-filtered (filter-options options new-text)))
    (make-completing-read-state
     new-text 0 new-filtered cursor-position #f #f
     (completing-read-state-defaults state)
     (completing-read-state-default-index state)
     (completing-read-state-inherit-input-method? state)
     (completing-read-state-history state)
     (completing-read-state-history-position state)
     (completing-read-state-history-start-position state)
     (completing-read-state-history-start-input state))))

(define (string-delete-last-word str)
  (let ((match (string-match "\\s*\\S+\\s*$" str)))
    (if match
        (regexp-substitute #f match 'pre)
        "")))

;; Handle selection of current option
(define (handle-select state)
  (let ((filtered-options (completing-read-state-filtered-options state))
        (selected-index (completing-read-state-selected-index state)))
    (if (and (not (null? filtered-options))
             (< selected-index (length filtered-options)))
        (list 'selected (list-ref filtered-options selected-index))
        (list 'no-change state))))

;; Return the highlighted position rather than its rendered text.  This is for
;; callers such as structured prompts whose display labels are not identifiers.
;; It deliberately shares the menu transition without changing ordinary
;; string-returning menu behavior.
(define (handle-select-index state)
  (let ((filtered-options (completing-read-state-filtered-options state))
        (selected-index (completing-read-state-selected-index state)))
    (if (and (not (null? filtered-options))
             (< selected-index (length filtered-options)))
        (list 'selected selected-index)
        (list 'no-change state))))

;; Submit either the editable input or the highlighted candidate.  Keeping
;; this transition independent of Wayland makes the public API's text/menu
;; boundary directly testable.
(define* (handle-submit state #:optional (selection-mode 'text)
                        #:key (collection #f) (predicate #f)
                        (require-match #f) (style 'substring))
  (case selection-mode
    ((text)
     (let* ((input (completing-read-state-input-text state))
            (defaults (completing-read-state-defaults state))
            (submitted-input (if (and (string-null? input) (pair? defaults))
                                 (car defaults)
                                 input))
            (decision
             (if (and (string-null? input) (pair? defaults))
                 (list 'accepted submitted-input)
                 (completion-submit
                  submitted-input collection require-match predicate
                  #:style style
                  #:completion-invoked?
                  (completing-read-state-completion-invoked? state)
                  #:confirmation-input
                  (completing-read-state-confirmation-input state)))))
       (case (car decision)
         ((accepted)
          (let ((history (completing-read-state-history state)))
            (when history
              (completion-history-add! history submitted-input))
            (list 'selected submitted-input)))
         ((confirm)
          (list 'state-update
                (make-completing-read-state
                 input
                 (completing-read-state-selected-index state)
                 (completing-read-state-filtered-options state)
                 (completing-read-state-cursor-position state)
                 input #f defaults
                 (completing-read-state-default-index state)
                 (completing-read-state-inherit-input-method? state)
                 (completing-read-state-history state)
                 (completing-read-state-history-position state)
                 (completing-read-state-history-start-position state)
                 (completing-read-state-history-start-input state))))
         ((rejected) (list 'no-change state)))))
    ((menu) (handle-select state))
    ((menu-index) (handle-select-index state))
    (else (error "unsupported selection mode" selection-mode))))

;; Handle moving selection down
(define (handle-next state)
  (let* ((selected-index (completing-read-state-selected-index state))
         (filtered-options (completing-read-state-filtered-options state))
         (count (length filtered-options)))
    (let ((new-idx (cond ((< (+ selected-index 1) count) (+ selected-index 1))
                         ((and (wrap-navigation?) (> count 0)) 0)
                         (else selected-index))))
      (list 'state-update
            (make-completing-read-state (completing-read-state-input-text state)
                                        new-idx
                                        filtered-options
                                        (completing-read-state-cursor-position state)
                                        #f #f
                                        (completing-read-state-defaults state)
                                        (completing-read-state-default-index state)
                                        (completing-read-state-inherit-input-method? state)
                                        (completing-read-state-history state)
                                        (completing-read-state-history-position state)
                                        (completing-read-state-history-start-position state)
                                        (completing-read-state-history-start-input state))))))

;; Handle moving selection up
(define (handle-previous state)
  (let* ((selected-index (completing-read-state-selected-index state))
         (filtered-options (completing-read-state-filtered-options state))
         (count (length filtered-options)))
    (let ((new-idx (cond ((> selected-index 0) (- selected-index 1))
                         ((and (wrap-navigation?) (> count 0)) (- count 1))
                         (else 0))))
      (list 'state-update
            (make-completing-read-state (completing-read-state-input-text state)
                                        new-idx
                                        filtered-options
                                        (completing-read-state-cursor-position state)
                                        #f #f
                                        (completing-read-state-defaults state)
                                        (completing-read-state-default-index state)
                                        (completing-read-state-inherit-input-method? state)
                                        (completing-read-state-history state)
                                        (completing-read-state-history-position state)
                                        (completing-read-state-history-start-position state)
                                        (completing-read-state-history-start-input state))))))

(define (move-cursor state position)
  (if (= position (completing-read-state-cursor-position state))
      (list 'no-change state)
      (list 'state-update
            (make-completing-read-state
             (completing-read-state-input-text state)
             (completing-read-state-selected-index state)
             (completing-read-state-filtered-options state)
             position #f #f
             (completing-read-state-defaults state)
             (completing-read-state-default-index state)
             (completing-read-state-inherit-input-method? state)
             (completing-read-state-history state)
             (completing-read-state-history-position state)
             (completing-read-state-history-start-position state)
             (completing-read-state-history-start-input state)))))

(define (handle-left state)
  (move-cursor state
               (max 0 (- (completing-read-state-cursor-position state) 1))))

(define (handle-right state)
  (move-cursor state
               (min (string-length (completing-read-state-input-text state))
                    (+ (completing-read-state-cursor-position state) 1))))

(define (handle-home state)
  (move-cursor state 0))

(define (handle-end state)
  (move-cursor state
               (string-length (completing-read-state-input-text state))))

;; Handle backspace - delete the character before the cursor.
(define* (handle-backspace state options #:key (ctrl-pressed? #f))
  (let ((input-text (completing-read-state-input-text state))
        (cursor (completing-read-state-cursor-position state)))
    (if (zero? cursor)
        (list 'no-change state)
        (let* ((prefix (substring input-text 0 cursor))
               (suffix (substring input-text cursor))
               (new-prefix (if ctrl-pressed?
                               (string-delete-last-word prefix)
                               (string-drop-right prefix 1)))
               (new-text (string-append new-prefix suffix)))
          (list 'state-update
                (update-text-and-filter state new-text options
                                        (string-length new-prefix)))))))

;; Handle character input
(define (handle-input-char char state options)
  (let* ((input-text (completing-read-state-input-text state))
         (cursor (completing-read-state-cursor-position state))
         (literal (string char))
         (inserted (if (completing-read-state-inherit-input-method? state)
                       ((completion-input-transformer) literal)
                       literal))
         (_ (unless (string? inserted)
              (error "completion input transformer did not return a string"
                     inserted)))
         (new-text (string-append (substring input-text 0 cursor)
                                  inserted
                                  (substring input-text cursor))))
    (list 'state-update
          (update-text-and-filter state new-text options
                                  (+ cursor (string-length inserted))))))

(define (handle-default state options offset)
  (let* ((defaults (completing-read-state-defaults state))
         (index (+ (completing-read-state-default-index state) offset)))
    (if (or (< index 0) (>= index (length defaults)))
        (list 'no-change state)
        (let* ((text (list-ref defaults index))
               (updated (update-text-and-filter state text options)))
          (list 'state-update
                (make-completing-read-state
                 text 0 (completing-read-state-filtered-options updated)
                 (string-length text) #f #f defaults index
                 (completing-read-state-inherit-input-method? state)
                 (completing-read-state-history state)
                 (completing-read-state-history-position state)
                 (completing-read-state-history-start-position state)
                 (completing-read-state-history-start-input state)))))))

(define (handle-next-default state options)
  (handle-default state options 1))

(define (handle-previous-default state options)
  (handle-default state options -1))

(define (navigation-state state options text history-position default-index)
  (let ((updated (update-text-and-filter state text options)))
    (make-completing-read-state
     text 0 (completing-read-state-filtered-options updated)
     (string-length text) #f #f
     (completing-read-state-defaults state) default-index
     (completing-read-state-inherit-input-method? state)
     (completing-read-state-history state) history-position
     (completing-read-state-history-start-position state)
     (completing-read-state-history-start-input state))))

(define (history-entry state position)
  (and (> position 0)
       (let ((history (completing-read-state-history state)))
         (and history
              (<= position (length (completion-history-entries history)))
              (list-ref (completion-history-entries history) (- position 1))))))

;; M-p moves into the newest-first real history.  When invoked from future
;; defaults, it first walks back through those defaults and then returns to the
;; original input/history position.
(define (handle-previous-history state options)
  (let ((default-index (completing-read-state-default-index state)))
    (cond
     ((> default-index 0)
      (list 'state-update
            (navigation-state state options
                              (list-ref (completing-read-state-defaults state)
                                        (- default-index 1))
                              (completing-read-state-history-position state)
                              (- default-index 1))))
     ((= default-index 0)
      (list 'state-update
            (navigation-state
             state options (completing-read-state-history-start-input state)
             (completing-read-state-history-start-position state) -1)))
     (else
      (let* ((position (+ (completing-read-state-history-position state) 1))
             (entry (history-entry state position)))
        (if entry
            (list 'state-update
                  (navigation-state state options entry position -1))
            (list 'no-change state)))))))

;; M-n moves toward position zero and then through DEFAULTS, the minibuffer's
;; future history.  Position zero restores the initial editable input.
(define (handle-next-history state options)
  (let ((default-index (completing-read-state-default-index state))
        (position (completing-read-state-history-position state))
        (defaults (completing-read-state-defaults state)))
    (cond
     ((>= default-index 0)
      (let ((next (+ default-index 1)))
        (if (< next (length defaults))
            (list 'state-update
                  (navigation-state state options (list-ref defaults next)
                                    position next))
            (list 'no-change state))))
     ((> position 0)
      (let* ((next (- position 1))
             (text (if (zero? next)
                       (completing-read-state-history-start-input state)
                       (history-entry state next))))
        (list 'state-update
              (navigation-state state options text next -1))))
     ((pair? defaults)
      (list 'state-update
            (navigation-state state options (car defaults) 0 0)))
     (else (list 'no-change state)))))

;; Attempt completion without coupling the transition to keyboard or Wayland
;; state.  Record every invocation because confirm-after-completion depends on
;; TAB being the immediately preceding action, even when it changes no text.
(define* (handle-complete state collection options
                          #:optional (predicate #f)
                          #:key (style 'substring))
  (let* ((input (completing-read-state-input-text state))
         (cursor (completing-read-state-cursor-position state))
         (prefix (substring input 0 cursor))
         (suffix (substring input cursor))
         (metadata (completion-metadata prefix collection predicate))
         (boundaries (completion-boundaries prefix collection predicate suffix))
         (field-start (car boundaries))
         (field-end (+ cursor (cdr boundaries)))
         (field (substring input field-start field-end))
         (completion (completion-try-completion
                      field collection predicate #:style style)))
    (if (and (string? completion)
             (not (string=? completion field)))
        (let* ((new-text (string-append (substring input 0 field-start)
                                        completion
                                        (substring input field-end)))
               (new-cursor (+ field-start (string-length completion)))
               (updated (update-text-and-filter state new-text options
                                                new-cursor)))
          (list 'state-update
                (make-completing-read-state
                 (completing-read-state-input-text updated)
                 (completing-read-state-selected-index updated)
                 (completing-read-state-filtered-options updated)
                 (completing-read-state-cursor-position updated)
                 #f #t
                 (completing-read-state-defaults state)
                 (completing-read-state-default-index state)
                 (completing-read-state-inherit-input-method? state)
                 (completing-read-state-history state)
                 (completing-read-state-history-position state)
                 (completing-read-state-history-start-position state)
                 (completing-read-state-history-start-input state)
                 metadata boundaries)))
        ;; Even a failed or already exact TAB matters to
        ;; `confirm-after-completion': it was the immediately preceding action.
        (list 'state-update
              (make-completing-read-state
               input
               (completing-read-state-selected-index state)
               (completing-read-state-filtered-options state)
               (completing-read-state-cursor-position state)
               #f #t
               (completing-read-state-defaults state)
               (completing-read-state-default-index state)
               (completing-read-state-inherit-input-method? state)
               (completing-read-state-history state)
               (completing-read-state-history-position state)
               (completing-read-state-history-start-position state)
               (completing-read-state-history-start-input state)
               metadata boundaries)))))
