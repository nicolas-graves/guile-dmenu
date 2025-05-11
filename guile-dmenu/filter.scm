(define-module (guile-dmenu filter)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:export (filter-options
            handle-select
            handle-move-down
            handle-move-up
            handle-backspace
            handle-input-char
            ;; State record exports
            make-completing-read-state
            completing-read-state?
            completing-read-state-input-text
            completing-read-state-selected-index
            completing-read-state-filtered-options
            initial-state))

;; Define a record type to hold completing-read state
(define-record-type <completing-read-state>
  (make-completing-read-state input-text selected-index filtered-options)
  completing-read-state?
  (input-text completing-read-state-input-text)
  (selected-index completing-read-state-selected-index)
  (filtered-options completing-read-state-filtered-options))

;; Create initial state
(define (initial-state options)
  (make-completing-read-state "" 0 options))

;; Filter options based on input text
(define (filter-options options input)
  (if (string-null? input)
      options
      (filter
       (lambda (opt)
         (string-contains-ci opt input))
       options)))

;; Helper to update state with new input text and reset selection
(define (update-text-and-filter state new-text options)
  (let ((new-filtered (filter-options options new-text)))
    (make-completing-read-state new-text 0 new-filtered)))

;; Handle selection of current option
(define (handle-select state)
  (let ((filtered-options (completing-read-state-filtered-options state))
        (selected-index (completing-read-state-selected-index state)))
    (if (and (not (null? filtered-options))
             (< selected-index (length filtered-options)))
        (let ((selected (list-ref filtered-options selected-index)))
          (format #t "~a~%" selected)
          (list 'exit 0))
        (list 'no-change state))))

;; Handle moving selection down
(define (handle-move-down state)
  (let ((selected-index (completing-read-state-selected-index state))
        (filtered-options (completing-read-state-filtered-options state)))
    (let ((new-idx (if (< (+ selected-index 1) (length filtered-options))
                       (+ selected-index 1)
                       selected-index)))
      (list 'state-update
            (make-completing-read-state (completing-read-state-input-text state)
                                        new-idx
                                        filtered-options)))))

;; Handle moving selection up
(define (handle-move-up state)
  (let ((selected-index (completing-read-state-selected-index state)))
    (let ((new-idx (if (> selected-index 0)
                       (- selected-index 1)
                       0)))
      (list 'state-update
            (make-completing-read-state (completing-read-state-input-text state)
                                        new-idx
                                        (completing-read-state-filtered-options state))))))

;; Handle backspace - delete last character
(define (handle-backspace state options)
  (let ((input-text (completing-read-state-input-text state)))
    (if (string-null? input-text)
        (list 'no-change state)
        (let ((new-text (string-drop-right input-text 1)))
          (list 'state-update (update-text-and-filter state new-text options))))))

;; Handle character input
(define (handle-input-char char state options)
  (let* ((input-text (completing-read-state-input-text state))
         (new-text (string-append input-text (string char))))
    (list 'state-update (update-text-and-filter state new-text options))))
