#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (advent grid)
        (advent input)
        (advent main))

;; Tiles in the grid
(define BEAM #\|)
(define EMPTY #\.)
(define SPLITTER #\^)
(define START #\S)

;; ---------------------------------------------------------------------------
;; Part 1

(define (propagate! grid)
  (grid-for-each
   (lambda (n m this)
     (let ((up (grid-ref grid (- n 1) m))
           (left (grid-ref grid n (- m 1)))
           (right (grid-ref grid n (+ m 1))))
       (cond
        ((eqv? this START)
         (grid-set! grid n m BEAM))
        ((and (eqv? this EMPTY)
              (eqv? up BEAM))
         (grid-set! grid n m BEAM))
        ((and (eqv? this SPLITTER)
              (eqv? up BEAM))
         (grid-set! grid n (- m 1) BEAM)
         (grid-set! grid n (+ m 1) BEAM)))))
   grid))

(define (part-1)
  (let ((grid (lines->grid (read-lines))))
    (propagate! grid)
    (display
     (grid-fold
      (lambda (n m num this)
        (if (and (eqv? this SPLITTER)
                 (eqv? (grid-ref grid (- n 1) m) BEAM))
            (+ num 1)
            num))
      0 grid))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (num-timelines! grid n m)
  (let ((this (grid-ref grid n m)))
    (cond
     ((number? this) this)
     ((or (eqv? this EMPTY)
          (eqv? this START))
      (num-timelines! grid (+ n 1) m))
     ((eqv? this SPLITTER)
      (let ((num (+ (num-timelines! grid n (- m 1))
                    (num-timelines! grid n (+ m 1)))))
        (grid-set! grid n m num)
        num))
     ((eqv? this #f) 1))))

(define (part-2)
  (let ((grid (lines->grid (read-lines))))
    (let-values (((n m) (grid-index (lambda (c)
                                      (char=? c START))
                                    grid)))
      (display (num-timelines! grid n m))
      (newline))))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
