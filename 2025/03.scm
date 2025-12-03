(import (srfi 13))

(define (string->battery-bank s)
  (let ((b (make-vector (string-length s))))
    (string-for-each-index
     (lambda (i)
       (vector-set! b i
                    (- (char->integer (string-ref s i))
                       (char->integer #\0))))
     s)
    b))
