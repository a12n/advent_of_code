(define-library (advent list)
  (export combinations fold-combinations)
  (import (scheme base)
          (srfi 1))
  (begin

    (define (fold-combinations f acc list)
      (if (null? list) acc
          (fold-combinations
           f
           (fold (lambda (b acc) (f (cons (car list) b) acc)) acc (cdr list))
           (cdr list))))

    (define (combinations list)
      (reverse (fold-combinations cons '() list)))

    ))
