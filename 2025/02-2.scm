#!/usr/bin/env gsi-script

(define (aux-iid? k m)
  (let ((d (expt 10 m)))
    (define (loop rest prev)
      (if (zero? rest) #t
          (receive (rest next)
              (truncate/ rest d)
            (if (= prev next)
                (loop rest next)
                #f))))
    (receive (rest prev)
        (truncate/ k d)
      (loop rest prev)))

(define (iid? k)
  (let ((n (ndigits k)))
    (define (loop m d)
      (cond
       ((> m n) #f)
       ((and (zero? (remainder n m)) (aux-iid? k (quotient n m))) #t)
       (else (loop (+ m 1)))))
    (loop 2 )))
