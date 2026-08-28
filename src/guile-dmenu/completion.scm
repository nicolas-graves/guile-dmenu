(define-module (guile-dmenu completion)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-completion-history
            completion-history?
            completion-history-entries
            set-completion-history-entries!
            completion-history-length
            completion-history-add!
            normalize-history
            completion-candidate?
            completion-candidate-display
            completion-candidate-entry
            completion-ignore-case?
            completion-input-transformer
            register-completion-style!
            lookup-completion-style
            completion-all-completions
            completion-try-completion
            completion-exact-match?
            completion-metadata
            completion-boundaries
            completion-submit
            run-completion-action
            normalize-collection
            call-completion-table))

;; Histories are explicit mutable values instead of symbol-bound globals.  The
;; newest entry is first, matching the ordering used by Emacs minibuffer
;; histories and making one-based navigation positions natural.
(define-record-type <completion-history>
  (%make-completion-history entries)
  completion-history?
  (entries completion-history-entries set-completion-history-entries!))

(define* (make-completion-history #:optional (entries '()))
  (unless (and (list? entries) (every string? entries))
    (error "completion history entries are not a list of strings" entries))
  (%make-completion-history entries))

;; #f means unlimited.  A parameter keeps limit changes dynamically scoped in
;; callers and tests, like the other completion settings in this module.
(define completion-history-length
  (make-parameter
   100
   (lambda (value)
     (unless (or (not value)
                 (and (integer? value) (exact? value) (>= value 0)))
       (error "completion history length is not a nonnegative integer or #f"
              value))
     value)))

(define (completion-history-add! history input)
  "Record nonempty INPUT in HISTORY and return its resulting entries.

Consecutive duplicates are suppressed and `completion-history-length' limits
the retained newest-first list."
  (unless (completion-history? history)
    (error "not a completion history" history))
  (unless (string? input)
    (error "completion history input is not a string" input))
  (let ((entries (completion-history-entries history)))
    (unless (and (list? entries) (every string? entries))
      (error "completion history entries are not a list of strings" entries))
    (let* ((updated
            (if (or (string-null? input)
                    (and (pair? entries) (string=? input (car entries))))
                entries
                (cons input entries)))
           (limit (completion-history-length))
           (limited (if (and limit (> (length updated) limit))
                        (take updated limit)
                        updated)))
      (unless (equal? limited entries)
        (set-completion-history-entries! history limited)))
    (completion-history-entries history)))

(define (normalize-history history)
  "Return HISTORY's mutable object and initial navigation position.

HISTORY may be #f, #t (which explicitly disables recording), a completion
history object, or (HISTORY . ONE-BASED-POSITION)."
  (cond
   ((or (not history) (eq? history #t)) (values #f 0))
   ((completion-history? history) (values history 0))
   ((pair? history)
    (let ((object (car history)) (position (cdr history)))
      (unless (completion-history? object)
        (error "positioned history does not contain a completion history"
               history))
      (unless (and (integer? position) (exact? position) (> position 0)
                   (<= position
                       (length (completion-history-entries object))))
        (error "history position is outside the history" position))
      (values object position)))
   (else (error "unsupported completion history" history))))

(define completion-ignore-case? (make-parameter #t))

;; Model inherited input methods as a pure string transformation.  Completion
;; states decide whether to consult this ambient parameter, so callers that do
;; not request inheritance always receive literal keyboard input.
(define completion-input-transformer (make-parameter (lambda (input) input)))

;; A style is a pure procedure of INPUT, normalized CANDIDATES, and an
;; Emacs-shaped ACTION.  Keeping the registry this small lets later styles be
;; added without coupling them to the Wayland event loop.
(define completion-styles (make-hash-table))

(define (register-completion-style! name style)
  (unless (symbol? name)
    (error "completion style name is not a symbol" name))
  (unless (procedure? style)
    (error "completion style is not a procedure" style))
  (hash-set! completion-styles name style)
  name)

(define (lookup-completion-style name)
  (or (hash-ref completion-styles name)
      (error "unknown completion style" name)))

;; Keep the text shown by the menu separate from the collection object passed
;; to a predicate.  In particular, an alist candidate displays its key while
;; retaining the complete entry (and therefore its value).
(define-record-type <completion-candidate>
  (make-completion-candidate display entry)
  completion-candidate?
  (display completion-candidate-display)
  (entry completion-candidate-entry))

(define (candidate display entry predicate)
  (unless (string? display)
    (error "completion candidate is not a string" display))
  (and (or (not predicate) (predicate entry))
       (make-completion-candidate display entry)))

(define (normalize-sequence entries entry->display predicate)
  (filter-map (lambda (entry)
                (candidate (entry->display entry) entry predicate))
              entries))

(define (alist? value)
  (and (list? value)
       (not (null? value))
       (every pair? value)))

(define (normalize-concrete-collection collection predicate)
  (cond
   ((vector? collection)
    (normalize-sequence (vector->list collection)
                        (lambda (entry) entry)
                        predicate))
   ((hash-table? collection)
    (normalize-sequence
     (hash-map->list (lambda (key value) (cons key value)) collection)
     car
     predicate))
   ((alist? collection)
    (normalize-sequence collection car predicate))
   ((list? collection)
    (normalize-sequence collection (lambda (entry) entry) predicate))
   (else
    (error "unsupported completion collection" collection))))

;; Keep this call as a deliberately thin boundary.  A programmed completion
;; table owns predicate interpretation and must receive the caller's objects
;; unchanged for every documented Emacs-style action.
(define (call-completion-table table input predicate action)
  (unless (procedure? table)
    (error "completion table is not a procedure" table))
  (table input predicate action))

(define* (normalize-collection collection #:optional (predicate #f)
                               (input "") (action #t))
  "Return display/source candidate records for COLLECTION.

String lists and vectors retain each string as their source entry.  Alists
display string keys and retain complete entries.  Hash tables behave like
alists with synthesized (KEY . VALUE) entries.  A programmed TABLE is called
with INPUT, PREDICATE, and ACTION exactly as supplied; its collection result is
then normalized without applying PREDICATE a second time."
  (if (procedure? collection)
      (normalize-concrete-collection
       (call-completion-table collection input predicate action)
       #f)
      (normalize-concrete-collection collection predicate)))

(define (completion-string=? left right)
  ((if (completion-ignore-case?) string-ci=? string=?) left right))

(define (completion-string-contains? candidate input)
  (if (completion-ignore-case?)
      (string-contains-ci candidate input)
      (string-contains candidate input)))

(define (substring-matches input candidates)
  (filter (lambda (candidate)
            (completion-string-contains?
             (completion-candidate-display candidate) input))
          candidates))

(define (common-prefix strings)
  (if (null? strings)
      ""
      (let ((first (car strings)))
        (let loop ((length 0))
          (if (or (= length (string-length first))
                  (any (lambda (other)
                         (or (= length (string-length other))
                             (not (completion-string=?
                                   (substring first length (+ length 1))
                                   (substring other length (+ length 1))))))
                       (cdr strings)))
              (substring first 0 length)
              (loop (+ length 1)))))))

(define (input-prefix? input value)
  (and (<= (string-length input) (string-length value))
       (completion-string=? input
                            (substring value 0 (string-length input)))))

(define (substring-style input candidates action)
  (let* ((matches (substring-matches input candidates))
         (displays (map completion-candidate-display matches)))
    (cond
     ((eq? action #t) displays)
     ((eq? action 'lambda)
      (any (lambda (display) (completion-string=? input display)) displays))
     ((not action)
      (cond
       ((null? displays) #f)
       ((null? (cdr displays))
        ;; Matching may ignore case, but the candidate remains the canonical
        ;; spelling to insert.  Only an exact spelling is already complete.
        (if (string=? input (car displays)) #t (car displays)))
       (else
        (let ((prefix (common-prefix displays)))
          ;; Substring matches need not begin with INPUT.  Only a shared prefix
          ;; that genuinely extends INPUT is safe to insert.
          (if (and (input-prefix? input prefix)
                   (> (string-length prefix) (string-length input)))
              prefix
              input)))))
     (else (error "unsupported completion action" action)))))

(register-completion-style! 'substring substring-style)

(define* (run-completion-action input collection action
                                #:optional (predicate #f)
                                #:key (style 'substring))
  "Run ACTION for INPUT against COLLECTION using STYLE.

ACTION is #t for all matching display strings, #f for try-completion, or
`lambda' for exact-match testing.  Programmed completion tables receive the
action directly; concrete collections are handled by the selected pure style."
  (unless (string? input)
    (error "completion input is not a string" input))
  (if (procedure? collection)
      (call-completion-table collection input predicate action)
      ((lookup-completion-style style)
       input (normalize-collection collection predicate) action)))

(define* (completion-all-completions input collection
                                     #:optional (predicate #f)
                                     #:key (style 'substring))
  (run-completion-action input collection #t predicate #:style style))

(define* (completion-try-completion input collection
                                    #:optional (predicate #f)
                                    #:key (style 'substring))
  (run-completion-action input collection #f predicate #:style style))

(define* (completion-exact-match? input collection
                                  #:optional (predicate #f)
                                  #:key (style 'substring))
  (run-completion-action input collection 'lambda predicate #:style style))

(define* (completion-metadata input collection #:optional (predicate #f))
  "Return COLLECTION's normalized `(metadata . ALIST)' response for INPUT.

Concrete collections and programmed tables that do not return a metadata
response have empty metadata.  PREDICATE is forwarded unchanged to programmed
tables."
  (unless (string? input)
    (error "completion input is not a string" input))
  (let ((response (and (procedure? collection)
                       (call-completion-table collection input predicate
                                              'metadata))))
    (if (and (pair? response) (eq? (car response) 'metadata)
             (list? (cdr response)) (every pair? (cdr response)))
        response
        '(metadata))))

(define (completion-boundaries input collection predicate suffix)
  "Return `(START . END)' for the completion field around point.

INPUT is the text before point and SUFFIX is the text after point.  Programmed
tables receive `(boundaries . SUFFIX)'.  Missing or malformed responses use
the concrete-collection boundary `(0 . (string-length SUFFIX))'."
  (unless (string? input)
    (error "completion input is not a string" input))
  (unless (string? suffix)
    (error "completion suffix is not a string" suffix))
  (let* ((response (and (procedure? collection)
                        (call-completion-table
                         collection input predicate (cons 'boundaries suffix))))
         (start (and (pair? response)
                     (eq? (car response) 'boundaries)
                     (pair? (cdr response))
                     (cadr response)))
         (end (and start (cddr response)))
         (normalized-start
          (if (and (integer? start) (exact? start)
                   (<= 0 start (string-length input)))
              start
              0))
         (normalized-end
          (if (and (integer? end) (exact? end)
                   (<= 0 end (string-length suffix)))
              end
              (string-length suffix))))
    (cons normalized-start normalized-end)))

(define* (completion-submit input collection require-match
                            #:optional (predicate #f)
                            #:key (style 'substring)
                            (completion-invoked? #f)
                            (confirmation-input #f))
  "Return the pure submission decision for INPUT.

The result is `(accepted INPUT)', `(confirm INPUT)', or `(rejected INPUT)'.
CONFIRMATION-INPUT is the input retained after a previous `confirm' decision.
For `confirm-after-completion', COMPLETION-INVOKED? records that TAB was the
immediately preceding input action.  A procedure REQUIRE-MATCH is the complete
acceptance predicate and is therefore consulted even for exact matches."
  (unless (string? input)
    (error "completion input is not a string" input))
  (cond
   ((not require-match) (list 'accepted input))
   ((procedure? require-match)
    (list (if (require-match input) 'accepted 'rejected) input))
   ((completion-exact-match? input collection predicate #:style style)
    (list 'accepted input))
   ((eq? require-match 'confirm)
    (list (if (and (string? confirmation-input)
                   (string=? input confirmation-input))
              'accepted
              'confirm)
          input))
   ((eq? require-match 'confirm-after-completion)
    (list (cond
           ((and (string? confirmation-input)
                 (string=? input confirmation-input))
            'accepted)
           (completion-invoked? 'confirm)
           (else 'accepted))
          input))
   (else (list 'rejected input))))
