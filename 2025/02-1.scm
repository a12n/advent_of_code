#!/usr/bin/env gsi-script

(import
 (srfi 1)
 (srfi 13)
 (srfi 14))

(include "aoc.scm")

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

;; Parse identifier range. Parsed ranges are represented as cons
;; cell. E.g., "11-22" will be parsed as (11 . 22).
(define (string->id-range s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\-)))))
    (if (= (length fields) 2)
        (cons (string->number (car fields))
              (string->number (cadr fields)))
        (error "invalid string" s))))

(assert (equal? (string->id-range "11-22") '(11 . 22)))
(assert (equal? (string->id-range "446443-446449") '(446443 . 446449)))

;; Parse comma-separated list of identifier ranges.
(define (string->id-range-list s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\,)))))
    (map string->id-range fields)))

(assert (equal? (string->id-range-list "") '()))
(assert (equal? (string->id-range-list "11-22,95-115,998-1012,1188511880-1188511890,222220-222224")
                '((11 . 22) (95 . 115) (998 . 1012) (1188511880 . 1188511890) (222220 . 222224))))

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
