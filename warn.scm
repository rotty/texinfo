(define texinfo-warn-handler
  (make-parameter (lambda (msg)
                    (format (current-error-port) "texinfo warning: ~s" msg))))

(define (texinfo-warn msg)
  ((texinfo-warn-handler) msg))

;; arch-tag: d5ebfe37-c23a-406f-8e92-8478bc637650
