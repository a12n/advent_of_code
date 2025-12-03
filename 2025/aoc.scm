(define-syntax assert
  (syntax-rules ()
    ((assert exp)
     (if exp #t (error "assert" (quote exp))))))
