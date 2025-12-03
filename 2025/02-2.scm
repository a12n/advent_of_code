#!/usr/bin/env gsi-script

(include "02.scm")

(define (aux-iid? k d)
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
    (define (loop m)
      (cond
       ((> m n) #f)
       ((and (zero? (remainder n m)) (aux-iid? k (expt 10 (quotient n m)))) #t)
       (else (loop (+ m 1)))))
    (loop 2)))

;; (assert (iid? 11))
;; (assert (iid? 22))
;; (assert (iid? 99))
;; (assert (iid? 111))
;; (assert (iid? 999))
;; (assert (iid? 1188511885))
;; (assert (iid? 565656))
;; (assert (not (iid? 123456)))

(define (fold-iid-range f acc r)
  (let* ((from (car r))
         (to (cdr r)))
    (define (loop id acc)
      (if (> id to) acc
          (loop (+ id 1)
                (if (iid? id) (f id acc) acc))))
    (loop from acc)))

(display
 (fold
  (lambda (r acc)
    (fold-iid-range + acc r))
  0
  (string->id-range-list (read-line))))
(newline)
