#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 28)
        (srfi 43)
        (srfi 117)
        (advent input)
        (advent grid)
        (advent main))

;; ---------------------------------------------------------------------------
;; Part 1

(define (beam-splitting! grid)
  (grid-fold
   (lambda (n m beam this)
     (let ((up (grid-ref grid (- n 1) m))
           (left (grid-ref grid n (- m 1)))
           (right (grid-ref grid n (+ m 1))))
       (cond
        ((eqv? this #\S)
         (grid-set! grid n m beam)
         (+ beam 1))

        ((and (eqv? this #\.)
              (number? up))
         (grid-set! grid n m up)
         beam)

        ((and (eqv? this #\.)
              (eqv? left #\^)           ; XXX
              (number? (grid-ref grid (- n 1) (- m 1))))
         (grid-set! grid n m beam)
         (+ beam 1))

        ((and (eqv? this #\.)
              (eqv? left #\>))
         (grid-set! grid n m (- beam 1))
         beam)

        ((and (eqv? this #\.)
              (eqv? right #\^)
              (number? (grid-ref grid (- n 1) (+ m 1))))
         (grid-set! grid n m beam)
         (grid-set! grid n (+ m 1) #\>)
         (+ beam 1))

        (else beam))))
   1 grid))

(define (part-1)
  (let ((grid (lines->grid (read-lines))))
    (display (beam-splitting! grid))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
