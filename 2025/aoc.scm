;; TODO: define-syntax
(define (assert equal? actual expected)
  (if (not (equal? actual expected))
      (error "assert" actual expected)))
