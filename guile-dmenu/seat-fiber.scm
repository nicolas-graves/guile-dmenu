(define-public (guile-dmenu seat-fiber)
  #:use-module (ice-9 records)
  )

(define-record-type <seat-fiber-context>
  (make-seat-fiber-context
   capabilities-channel
   name-channel
   keyboard-channel
   pointer-channel
   touch-channel)
  seat-fiber-context?
  (capabilities-channel seat-capabilities-channel)
  (name-channel seat-name-channel)
  (keyboard-channel seat-keyboard-channel)
  (pointer-channel seat-pointer-channel)
  (touch-channel seat-touch-channel))

;; Create a fiber-based seat listener
(define (create-fiber-seat-listener seat-context)
  (make <wl-seat-listener>
    #:capabilities
    (lambda (data seat capabilities)
      ;; Put the event on the capabilities channel
      (put-message (seat-capabilities-channel seat-context)
                   (list seat capabilities)))

    #:name
    (lambda (data seat name)
      ;; Put the event on the name channel
      (put-message (seat-name-channel seat-context)
                   (list seat name)))))

;; Initialize keyboard when capabilities indicate it's available
(define (handle-seat-capabilities seat-context seat capabilities)
  ;; Check if keyboard capability is available (bit 1)
  (when (logand capabilities 2)
    (let ((keyboard (wl-seat-get-keyboard seat)))
      ;; Store keyboard reference and set up its listener
      (put-message (seat-keyboard-channel seat-context) keyboard)
      ;; Create keyboard fiber context
      (let ((kbd-context (create-keyboard-fiber-context)))
        (wl-keyboard-add-listener keyboard
                                  (create-fiber-keyboard-listener kbd-context))
        kbd-context))))
