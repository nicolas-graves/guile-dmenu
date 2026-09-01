(define-module (guile-dmenu menu)
  #:use-module (guile-dmenu startup)
  #:use-module (guile-dmenu wayland)
  #:use-module (guile-dmenu graphics)
  #:use-module (guile-dmenu keyboard)
  #:use-module (guile-dmenu filter)
  #:use-module (guile-dmenu completion)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)
  #:use-module (ice-9 match)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers timers)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (completing-read
            completing-read/result
            completing-read-result?
            completing-read-result-status
            completing-read-result-value
            completing-read-result-status-procedure
            completing-read-result-value-procedure
            menu-width menu-padding menu-max-options)
  #:re-export (make-completion-history
               completion-history?
               completion-history-entries
               set-completion-history-entries!
               completion-history-length
               completion-history-add!))

(define menu-width (make-parameter 800))
(define menu-padding (make-parameter 4))
;; Keep the default surface bounded even for very large candidate sets.
;; Callers and the dmenu -l option can still choose a different limit.
(define menu-max-options (make-parameter 10))

(define-record-type <completing-read-result>
  (make-completing-read-result status value)
  completing-read-result?
  (status completing-read-result-status)
  (value completing-read-result-value))

(define (answered value) (make-completing-read-result 'answered value))
(define (terminated status) (make-completing-read-result status #f))

;; SRFI-9 accessors are syntax transformers in Guile.  These procedural
;; bridges support callers that resolve the graphical module lazily.
(define (completing-read-result-status-procedure result)
  (completing-read-result-status result))
(define (completing-read-result-value-procedure result)
  (completing-read-result-value result))

(define (draw-state prompt message-lines option-details details-visible?
                    input-enabled? conn cache scale state maximum
                    comment-state comment-index)
  (let ((surface (wayland-connection-surface conn))
        (commenting? (and comment-state #t))
        (state-message-lines
         (append
          (if comment-state
              (cons "Comment on selected choice  ·  Enter attach  ·  Esc back"
                    ;; Structured prompts put general shortcuts first and
                    ;; caller context afterwards.  Replace only the shortcuts
                    ;; while the editor is active.
                    (if (pair? message-lines) (cdr message-lines) '()))
              message-lines)
          ;; Read-only structured menus keep the selected option's description
          ;; visible.  Editable completion menus retain the explicit details
          ;; toggle so ordinary dmenu rows stay compact.
          (if (and (or details-visible? (not input-enabled?))
                   (pair? option-details))
              (let* ((index (completing-read-state-selected-index state))
                     (detail (and (< index (length option-details))
                                  (list-ref option-details index))))
                (if detail
                    (wrap-message-to-width
                     detail (menu-width) (menu-padding) 6)
                    '()))
              '())
          (if (completing-read-state-confirmation-input state)
              '("Confirm submission with RET")
              '()))))
    (when surface
      (let ((buffer (draw-menu (menu-width) 0 (menu-padding) cache
                               (if commenting? "Comment ›" prompt)
                               (if commenting?
                                   (completing-read-state-input-text comment-state)
                                   (completing-read-state-input-text state))
                               (completing-read-state-selected-index state)
                               (completing-read-state-filtered-options state)
                               maximum #:message-lines state-message-lines
                               #:scale scale
                               #:cursor-position
                               (completing-read-state-cursor-position
                                (if commenting? comment-state state))
                               #:input-enabled? (or commenting? input-enabled?))))
        (when buffer
          (wl-surface-attach surface buffer 0 0)
          (wl-surface-damage surface 0 0 (menu-width) 10000)
          (wl-surface-commit surface)
          (report-startup-phase! 'first-frame-committed))))))

(define* (completing-read/result prompt collection
                          #:optional (predicate #f) (require-match #f)
                          (initial-input "") (history #f) (default #f)
                          (inherit-input-method #f)
                          #:key (message #f) (input-enabled? #t)
                          (option-details #f)
                          (comment-on-tab? #f)
                          (escape-action 'cancel)
                          (timeout #f) (max-message-lines 12)
                          (selection-mode 'text)
                          (initial-selected-index 0)
                          (completion-style 'substring))
  "Display COLLECTION and return a structured termination result.
INITIAL-INPUT may be a string or (STRING . ZERO-BASED-POSITION).
DEFAULT may be a string or list of strings.  Empty input returns its first
value.  INHERIT-INPUT-METHOD enables `completion-input-transformer' for typed
input.  HISTORY may be a mutable completion history, a positioned history
pair, #f, or #t to disable recording.
SELECTION-MODE may be `text' (the default), `menu' to return the highlighted
string, or `menu-index' to return its zero-based displayed position.
INITIAL-SELECTED-INDEX controls the initially highlighted displayed candidate.
ESCAPE-ACTION is `cancel' by default; `back' returns the symbol `back'."
  (unless (memq selection-mode '(text menu menu-index))
    (error "unsupported selection mode" selection-mode))
  (unless (boolean? comment-on-tab?)
    (error "comment-on-tab marker must be boolean" comment-on-tab?))
  (unless (memq escape-action '(cancel back))
    (error "escape action must be cancel or back" escape-action))
  ;; Resolve the style before opening a Wayland connection so an invalid
  ;; option fails deterministically even when TAB is never pressed.
  (lookup-completion-style completion-style)
  ;; Validate initial input before connecting to Wayland so malformed public
  ;; calls fail synchronously and without compositor side effects.
  (let ((normalized-initial
         (call-with-values
             (lambda () (normalize-initial-input initial-input)) list)))
    (normalize-defaults default)
    (call-with-values (lambda () (normalize-history history)) list)
    (let* ((candidates (normalize-collection collection predicate
                                            (car normalized-initial)))
         (options (map completion-candidate-display candidates))
         (maximum (or (menu-max-options) (length options)))
         (message-lines (if message
                            (wrap-message-to-width message
                                                   (menu-width)
                                                   (menu-padding)
                                                   max-message-lines)
                            '())))
    (report-startup-phase! 'collection-normalized)
    (unless (or (not option-details)
                (and (list? option-details)
                     (= (length option-details) (length options))
                     (every (lambda (detail)
                              (or (not detail) (string? detail)))
                            option-details)))
      (error "option details must match the displayed collection"
             option-details))
    (run-fibers
     (lambda ()
       ;; Startup runs inside the scheduler's main fiber.  An uncaught
       ;; exception there only terminates that fiber; RUN-FIBERS can continue
       ;; waiting on the scheduler indefinitely.  Convert startup failures to
       ;; the same terminal result used by event-loop failures.
       (catch #t
         (lambda ()
           (let ((session (make-keyboard-session))
                 (events (make-channel))
                 (result (make-channel)))
         (set-keyboard-session-key-handler!
          session (make-key-decoder session events result
                                    #:cancel-as-event? #t))
         (let* ((conn (connect-wayland (make-seat-listener session)))
                (display (wayland-connection-display conn))
                (port (fdes->inport (wl-display-get-fd display)))
                (_wayland-phase
                 (report-startup-phase! 'wayland-connected))
                (state (initial-state options initial-input default
                                      inherit-input-method history
                                      initial-selected-index))
                (_state-phase
                 (report-startup-phase! 'initial-state-constructed))
                (buffer-scale 1)
                (read-prepared? #f)
                (details-visible? #f)
                (cache (make-menu-buffer-cache
                        (wayland-connection-shm conn)
                        #:request-redraw
                        (lambda () (spawn-fiber (lambda ()
                                                  (put-message events 'redraw)))))))
           (define (redraw s comment-state comment-index)
             (draw-state prompt message-lines option-details details-visible?
                         input-enabled? conn cache buffer-scale s maximum
                         comment-state comment-index)
             (wl-display-flush display))
           (create-window
            conn "dmenu" "wl-dmenu" (menu-width)
            (lambda (data surface serial)
              (xdg-surface-ack-configure surface serial)
              (report-runtime-event! 'surface-configured)
              (spawn-fiber (lambda () (put-message events 'redraw))) #t)
            (lambda (data toplevel width height states)
              (unless (zero? width) (menu-width width)) #t)
            (lambda args
              (spawn-fiber (lambda () (put-message result '(window-closed))))
              #t)
            #:scale-callback
            (lambda (scale)
              (unless (= scale buffer-scale)
                (set! buffer-scale scale)
                (spawn-fiber (lambda () (put-message events 'redraw))))
              #t))
           (report-startup-phase! 'window-created)
           ;; Send the initial empty surface commit before waiting for the
           ;; compositor's configure event.  Redrawing before configure is a
           ;; protocol error, but waiting without flushing deadlocks startup.
           (wl-display-flush display)
           (spawn-fiber
            (lambda ()
              ;; A callback or drawing failure must wake the result waiter.
              ;; Otherwise this fiber can disappear while the main fiber (and
              ;; therefore the MCP question worker) remains blocked forever.
              (catch #t
                (lambda ()
                 (let loop ((s state) (comment-state #f) (comment-index #f))
                ;; Drain callbacks already queued by libwayland, then prepare
                ;; exactly one nonblocking socket read.  If another fiber's
                ;; event wins the race below, cancel that prepared read.
                (let prepare ()
                  (when (negative? (wl-display-prepare-read display))
                    (when (negative? (wl-display-dispatch-pending display))
                      (put-message result '(graphical-failure)))
                    (prepare)))
                (set! read-prepared? #t)
                (wl-display-flush display)
                (let ((event (perform-operation
                              (choice-operation
                               (get-operation events)
                               (wrap-operation (wait-until-port-readable-operation port)
                                               (lambda () 'wayland))))))
                  (unless (eq? event 'wayland)
                    (wl-display-cancel-read display)
                    (set! read-prepared? #f))
                  (match event
                    ((or 'redraw 'surface-configure)
                     (redraw s comment-state comment-index)
                     (loop s comment-state comment-index))
                    ('wayland (set! read-prepared? #f)
                              (if (or (negative? (wl-display-read-events display))
                                      (negative? (wl-display-dispatch-pending display)))
                                  (put-message result '(graphical-failure))
                                  (loop s comment-state comment-index)))
                    ('cancel
                     (if comment-state
                         (begin
                           (redraw s #f #f)
                           (loop s #f #f))
                         (put-message result
                                      (if (eq? escape-action 'back)
                                          '(answered back)
                                          '(cancelled)))))
                    ('select
                     (report-runtime-event! 'select-received)
                     (if comment-state
                         (let ((text (completing-read-state-input-text
                                      comment-state)))
                           (if (zero? (string-length text))
                               (loop s comment-state comment-index)
                               (put-message result
                                            `(answered
                                              (comment ,comment-index ,text)))))
                         (match (dispatch-completing-read-event
                                 s event options collection
                                 #:predicate predicate
                                 #:selection-mode selection-mode
                                 #:require-match require-match
                                 #:style completion-style
                                 #:input-enabled? input-enabled?)
                           (('selected value)
                            (put-message result `(answered ,value)))
                           (('state-update n) (redraw n #f #f) (loop n #f #f))
                           (_ (loop s #f #f)))))
                    ('complete
                     (cond
                      (comment-state
                       (loop s comment-state comment-index))
                      ((and (not input-enabled?) comment-on-tab?)
                         (let ((index (completing-read-state-selected-index s))
                               (editor (initial-state '() "" #f #f #f 0)))
                           (redraw s editor index)
                           (loop s editor index)))
                      ((and (not input-enabled?) option-details)
                       (set! details-visible? (not details-visible?))
                       (redraw s #f #f)
                       (loop s #f #f))
                      (else
                       (match (dispatch-completing-read-event
                               s event options collection
                               #:predicate predicate
                               #:selection-mode selection-mode
                               #:require-match require-match
                               #:style completion-style
                               #:input-enabled? input-enabled?)
                         (('state-update n) (redraw n #f #f) (loop n #f #f))
                         (_ (loop s #f #f))))))
                    (_
                     (match (dispatch-completing-read-event
                             (or comment-state s) event
                             (if comment-state '() options)
                             (if comment-state '() collection)
                             #:predicate predicate
                             #:selection-mode (if comment-state 'text selection-mode)
                             #:require-match (if comment-state #f require-match)
                             #:style completion-style
                             #:input-enabled? (or comment-state input-enabled?))
                       (('selected value) (put-message result `(answered ,value)))
                       (('state-update n)
                        (if comment-state
                            (begin (redraw s n comment-index)
                                   (loop s n comment-index))
                            (begin (redraw n #f #f) (loop n #f #f))))
                       (_ (loop s comment-state comment-index))))))))
                (lambda args
                  (report-runtime-event! 'event-loop-failure)
                  (put-message result '(graphical-failure))))))
           (let ((answer
                  (perform-operation
                   (if timeout
                       (choice-operation
                        (get-operation result)
                        (wrap-operation (sleep-operation timeout)
                                        (lambda () '(timed-out))))
                       (get-operation result)))))
             (report-runtime-event! 'result-received)
             (when read-prepared?
               (wl-display-cancel-read display)
               (set! read-prepared? #f))
             (wl-display-disconnect display)
             (match answer
               (('answered value) (answered value))
               (('cancelled) (terminated 'cancelled))
               (('timed-out) (terminated 'timed-out))
               (('window-closed) (terminated 'window-closed))
               (('graphical-failure) (terminated 'graphical-failure))
               (_ (terminated 'graphical-failure)))))))
         (lambda args
           (report-runtime-event! 'graphical-startup-failure)
           (terminated 'graphical-failure))))
      #:parallelism 1
      #:drain? #f))))

(define (completing-read . arguments)
  "Compatibility wrapper returning the answer, or #f on termination."
  (let ((result (apply completing-read/result arguments)))
    (and (eq? (completing-read-result-status result) 'answered)
         (completing-read-result-value result))))
