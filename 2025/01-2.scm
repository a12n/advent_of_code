#!/usr/bin/env gsi-script

(include "aoc.scm")
(include "01.scm")

;; Password method 0x434C49434B.
(define (rotate-click dial rotation)
  (receive (delta-rotation rotation)
      (truncate/ rotation 100)
    (let* ((dial-unlim (+ dial rotation))
           (delta-dial
            (cond
             ((and (> dial 0) (<= dial-unlim 0)) -1)
             ((>= dial-unlim 100) +1)
             (else 0)))
           (dial (modulo dial-unlim 100)))
      (values dial (+ (abs delta-rotation) (abs delta-dial))))))

(display (entrance-password rotate-click 50 0))
(newline)

;; Test cases for rotate-click
(let ((rotate-test
       (lambda (dial rotation)
         (receive (dial-rotated delta)
             (rotate-click dial rotation)
           (cons dial-rotated delta)))))
  (assert (equal? (rotate-test 50 -68) '(82 . 1)))
  (assert (equal? (rotate-test 82 -30) '(52 . 0)))
  (assert (equal? (rotate-test 52 +48) '(0 . 1)))
  (assert (equal? (rotate-test 0 -5) '(95 . 0)))
  (assert (equal? (rotate-test 95 +60) '(55 . 1)))
  (assert (equal? (rotate-test 55 -55) '(0 . 1)))
  (assert (equal? (rotate-test 0 -1) '(99 . 0)))
  (assert (equal? (rotate-test 99 -99) '(0 . 1)))
  (assert (equal? (rotate-test 0 +14) '(14 . 0)))
  (assert (equal? (rotate-test 14 -82) '(32 . 1))))
