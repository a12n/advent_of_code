;; Converts dial rotation description (e.g., "L55" or "R48") to exact
;; numbers (e.g., -55 or +48).
(define (string->rotation s)
  (let ((t (string-copy s)))
    (string-set! t 0
                 (case (string-ref t 0)
                   ((#\L #\l) #\-)
                   ((#\R #\r) #\+)
                   (else (error "invalid direction" s))))
    (let ((r (string->number t)))
      (if (zero? r) (error "zero rotation" s) r))))

(define (entrance-password rotate dial password)
  (let ((line (read-line)))
    (if (eof-object? line)
        password
        (receive (dial delta)
            (rotate dial (string->rotation line))
          (entrance-password rotate dial (+ password delta))))))
