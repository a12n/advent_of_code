#!/usr/bin/env gsi-script

(import (srfi 1))

(include "02.scm")

;; The number of digits in integer n written in base.
(define (ndigits n #!optional (base 10))
  (let ((m (quotient n base)))
    (if (zero? m) 1 (+ 1 (ndigits m)))))

(assert (= (ndigits 0) 1))
(assert (= (ndigits 1) 1))
(assert (= (ndigits 9) 1))
(assert (= (ndigits 10) 2))
(assert (= (ndigits 123) 3))
(assert (= (ndigits 131072) 6))

;; Minimum invalid identifier not less than the provided identifier k.
;;
;; Invalid identifiers represented as half of the digits of the actual
;; identifier. E.g., invalid identifier 123123 is represented as 123.
(define (ceiling-iid k)
  (let* ((n (ndigits k))
         (m (quotient n 2))
         (d (expt 10 m)))
    (if (even? n)
        (let* ((l (quotient k d))
               (r (remainder k d)))
          (if (>= l r)
               l
               (+ l 1)))
        d)))

(assert (= (ceiling-iid    98)   9)) ; 99
(assert (= (ceiling-iid   115)  10)) ; 1010
(assert (= (ceiling-iid  1015)  11)) ; 1111
(assert (= (ceiling-iid   998)  10)) ; 1010
(assert (= (ceiling-iid 91987) 100)) ; 100100
(assert (= (ceiling-iid   712)  10)) ; 1010
(assert (= (ceiling-iid   101)  10)) ; 1010

;; Convert invalid identifier to it's original number form. E.g.,
;; invalid identifier 123 (which represents identifier 123123) will be
;; converted to the original 123123.
(define (iid->number iid)
  (define (loop sum left)
    (if (zero? left)
        (+ sum iid)
        (loop (* sum 10) (quotient left 10))))
  (loop iid iid))

;; Left fold over all invalid identifiers in the range.
(define (fold-iid-range f acc r)
  (let* ((from (car r))
         (to (cdr r)))
    (define (loop iid acc)
      (let ((id (iid->number iid)))
        (if (> id to) acc
            (loop (+ iid 1) (f id acc)))))
    (loop (ceiling-iid from) acc)))

;; Make list of invalid identifiers in the range.
(define (iid-range->list r)
  (reverse (fold-iid-range cons '() r)))

(display
 (fold
  (lambda (r acc)
    (fold-iid-range + acc r))
  0
  (string->id-range-list (read-line))))
(newline)
