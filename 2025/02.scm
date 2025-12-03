(import
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
