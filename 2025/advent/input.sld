(define-library (advent input)
  (export fold-lines read-lines)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 13)
          (srfi 14))
  (begin

    (define fold-lines
      (case-lambda
       ((f acc) (fold-lines f acc (current-input-port)))
       ((f acc input)
        (define (loop acc)
          (let ((line (read-line input)))
            (if (eof-object? line) acc
                (loop (f line acc)))))
        (loop acc))))

    (define read-lines
      (case-lambda
       (() (read-lines (current-input-port)))
       ((input) (reverse (fold-lines cons '() input)))))

    ))
