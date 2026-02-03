#!/usr/bin/env gosh

(import
 (scheme base)
 (scheme char)
 (scheme write)
 (srfi 1)
 (advent main))

(define (string->battery-bank s)
  (map digit-value (string->list s)))

;;   '(2  3   4    2     3      4       2        3        4        2        3        4        2        7        8)
;;    ------------------------------------------------------------------------------------------------------------
;; 1 '(2  3   4    4     4      4       4        4        4        4        4        4        4        7        8)
;; 2 '(  23  34   42    43     44      44       44       44       44       44       44       44       47       78)
;; 3 '(     234  342   423    434     442      443      444      444      444      444      444      447      478)
;; 4 '(         2342  3423   4234    4342     4423     4434     4442     4443     4444     4444     4447     4478)
;; 5 '(              23423  34234   42342    43423    44234    44342    44423    44434    44442    44447    44478)
;; 6 '(                    234234  342342   423423   434234   442342   443423   444234   444342   444427   444478)
;; 7 '(                           2342342  3423423  4234234  4342342  4423423  4434234  4442342  4443427  4444278)
;; 8 '(                                   23423423 34234234 42342342 43423423 44234234 44342342 44423427 44434278)

(define (largest-joltage n bank)
  (define (loop m prev-row bank-left)
    (if (= m n) (last prev-row)
        (let ((next-rev
               (fold
                (lambda (prev-max battery next-acc)
                  (let ((next-elt (+ (* prev-max 10) battery)))
                    (if (or (null? next-acc) (> next-elt (car next-acc)))
                        (cons next-elt next-acc)
                        (cons (car next-acc) next-acc))))
                '() prev-row bank-left)))
          (loop (+ m 1) (reverse next-rev) (cdr bank-left)))))
  (loop 0 (circular-list 0) bank))

(define (total-output-joltage n sum)
  (let ((line (read-line)))
    (if (eof-object? line) sum
        (total-output-joltage
         n (+ sum (largest-joltage n (string->battery-bank line)))))))

(define (part-1)
  (display (total-output-joltage 2 0))
  (newline))

(define (part-2)
  (display (total-output-joltage 12 0))
  (newline))

(main part-1 part-2)
