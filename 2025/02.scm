#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (srfi 78)
        (advent main))

;; The number of digits in integer n written in base 10.
(define (ndigits n)
  (let ((m (quotient n 10)))
    (if (zero? m) 1 (+ 1 (ndigits m)))))

;; Parse identifier range. Parsed ranges are represented as cons
;; cell. E.g., "11-22" will be parsed as (11 . 22).
(define (string->id-range s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\-)))))
    (if (= (length fields) 2)
        (cons (string->number (car fields))
              (string->number (cadr fields)))
        (error "invalid string" s))))

;; Parse comma-separated list of identifier ranges.
(define (string->id-range-list s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\,)))))
    (map string->id-range fields)))

;; ---------------------------------------------------------------------------
;; Part 1

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

;; Convert invalid identifier to it's original number form. E.g.,
;; invalid identifier 123 (which represents identifier 123123) will be
;; converted to the original 123123.
(define (iid->number iid)
  (define (loop sum left)
    (if (zero? left)
        (+ sum iid)
        (loop (* sum 10) (quotient left 10))))
  (loop iid iid))

(define (part-1)
  ;; Left fold over all invalid identifiers in the range.
  (define (fold-iid-range f acc r)
    (let* ((from (car r))
           (to (cdr r)))
      (define (loop iid acc)
        (let ((id (iid->number iid)))
          (if (> id to) acc
              (loop (+ iid 1) (f id acc)))))
      (loop (ceiling-iid from) acc)))
  (display
   (fold
    (lambda (range acc)
      (fold-iid-range + acc range))
    0
    (string->id-range-list (read-line))))
  (newline))

;; ---------------------------------------------------------------------------
;; Part 2

(define (aux-iid? k d)
  (define (loop rest prev)
    (if (zero? rest) #t
        (let-values (((rest next) (truncate/ rest d)))
          (if (= prev next)
              (loop rest next)
              #f))))
  (let-values (((rest prev) (truncate/ k d)))
    (loop rest prev)))

(define (iid? k)
  (let ((n (ndigits k)))
    (define (loop m)
      (cond
       ((> m n) #f)
       ((and (zero? (remainder n m)) (aux-iid? k (expt 10 (quotient n m)))) #t)
       (else (loop (+ m 1)))))
    (loop 2)))

(define (part-2)
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
  (newline))

;; ---------------------------------------------------------------------------
;; Main

(define (test)
  ;; ndigits
  (check (ndigits 0) => 1)
  (check (ndigits 1) => 1)
  (check (ndigits 9) => 1)
  (check (ndigits 10) => 2)
  (check (ndigits 123) => 3)
  (check (ndigits 131072) => 6)
  ;; string->id-range
  (check (string->id-range "11-22") => '(11 . 22))
  (check (string->id-range "446443-446449") => '(446443 . 446449))
  ;; string->id-range-list
  (check (string->id-range-list "") => '())
  (check (string->id-range-list "11-22,95-115,998-1012,1188511880-1188511890,222220-222224") =>
         '((11 . 22) (95 . 115) (998 . 1012) (1188511880 . 1188511890) (222220 . 222224)))
  ;; ceiling-iid
  (check (ceiling-iid    98) =>   9)    ; 99
  (check (ceiling-iid   115) =>  10)    ; 1010
  (check (ceiling-iid  1015) =>  11)    ; 1111
  (check (ceiling-iid   998) =>  10)    ; 1010
  (check (ceiling-iid 91987) => 100)    ; 100100
  (check (ceiling-iid   712) =>  10)    ; 1010
  (check (ceiling-iid   101) =>  10)    ; 1010
  ;; iid?
  (check (iid? 11) => #t)
  (check (iid? 22) => #t)
  (check (iid? 99) => #t)
  (check (iid? 111) => #t)
  (check (iid? 999) => #t)
  (check (iid? 1188511885) => #t)
  (check (iid? 565656) => #t)
  (check (iid? 123456) => #f))

(main part-1 part-2 test)
