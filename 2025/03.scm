(define (string->battery-bank s)
  (list->vector (map digit-value (string->list s))))
