#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (advent grid)
        (advent input)
        (advent main))

;; ---------------------------------------------------------------------------
;; Part 1

(define (propagate! grid)
  (grid-for-each
   (lambda (n m this)
     (let ((up (grid-ref grid (- n 1) m))
           (left (grid-ref grid n (- m 1)))
           (right (grid-ref grid n (+ m 1))))
       (cond
        ((eqv? this #\S)
         (grid-set! grid n m #\|))
        ((and (eqv? this #\.)
              (eqv? up #\|))
         (grid-set! grid n m #\|))
        ((and (eqv? this #\^)
              (eqv? up #\|))
         (grid-set! grid n (- m 1) #\|)
         (grid-set! grid n (+ m 1) #\|)))))
   grid))

(define (part-1)
  (let ((grid (lines->grid (read-lines))))
    (propagate! grid)
    (display
     (grid-fold
      (lambda (n m num this)
        (if (and (eqv? this #\^)
                 (eqv? (grid-ref grid (- n 1) m) #\|))
            (+ num 1)
            num))
      0 grid))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
