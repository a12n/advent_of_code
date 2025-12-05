#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 78)
        (advent main))

;; Converts dial rotation description (e.g., "L55" or "R48") to exact
;; numbers (e.g., -55 or +48).
(define (string->rotation s)
  (let ((t (string-copy s)))
    (string-set! t 0
                 (case (string-ref t 0)
                   ((#\L #\l) #\-)
                   ((#\R #\r) #\+)
                   (else (error "invalid direction" s))))
    (let ((r (string->number t)))
      (if (zero? r) (error "zero rotation" s) r))))

;; Simple password method (the dial is pointing at 0 after a
;; rotation).
(define (rotate-simple dial rotation)
  (let* ((dial-rotated (modulo (+ dial rotation) 100))
         (delta (if (zero? dial-rotated) 1 0)))
    (values dial-rotated delta)))

;; Password method 0x434C49434B.
(define (rotate-click dial rotation)
  (let-values (((delta-rotation rotation) (truncate/ rotation 100)))
    (let* ((dial-unlim (+ dial rotation))
           (delta-dial
            (cond
             ((and (> dial 0) (<= dial-unlim 0)) -1)
             ((>= dial-unlim 100) +1)
             (else 0)))
           (dial (modulo dial-unlim 100)))
      (values dial (+ (abs delta-rotation) (abs delta-dial))))))

(define (entrance-password rotate dial password)
  (let ((line (read-line)))
    (if (eof-object? line)
        password
        (let-values (((dial delta) (rotate dial (string->rotation line))))
          (entrance-password rotate dial (+ password delta))))))

(define (part-1)
  (display (entrance-password rotate-simple 50 0))
  (newline))

(define (part-2)
  (display (entrance-password rotate-click 50 0))
  (newline))

(define (test)
  (define (rotate-click* dial rotation)
    (let-values (((dial-rotated delta) (rotate-click dial rotation)))
      (cons dial-rotated delta)))
  (check (rotate-click* 50 -68) => '(82 . 1))
  (check (rotate-click* 82 -30) => '(52 . 0))
  (check (rotate-click* 52 +48) => '(0 . 1))
  (check (rotate-click* 0 -5) => '(95 . 0))
  (check (rotate-click* 95 +60) => '(55 . 1))
  (check (rotate-click* 55 -55) => '(0 . 1))
  (check (rotate-click* 0 -1) => '(99 . 0))
  (check (rotate-click* 99 -99) => '(0 . 1))
  (check (rotate-click* 0 +14) => '(14 . 0))
  (check (rotate-click* 14 -82) => '(32 . 1)))

(main part-1 part-2 test)
