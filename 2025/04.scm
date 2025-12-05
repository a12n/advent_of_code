#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (advent grid)
        (advent input)
        (advent main))

(define (read-input)
  (let ((grid (lines->grid (read-lines))))
    (grid-map!
     (lambda (n m c)
       (char=? c #\@))
     grid)
    grid))

(define (num-neighbors grid n m)
  (fold-grid-neighbors-8
   (lambda (dn dm num)
     (+ num (if (grid-ref grid (+ n dn) (+ m dm)) 1 0)))
   0))

(define (num-accessible grid)
  (grid-fold
   (lambda (n m num paper?)
     (+ num (if (and paper?
                     (< (num-neighbors grid n m) 4))
              1 0)))
   0 grid))

(define (part-1)
  (display (num-accessible (read-input)))
  (newline))

(define (remove-accessible! grid)
  (grid-fold (lambda (n m num paper?)
               (+ num (if (and paper? (< (num-neighbors grid n m) 4))
                          (begin (grid-set! grid n m #f) 1)
                          0)))
             0 (grid-copy grid)))

(define (remove-all-accessible! grid num-total)
  (let ((num-removed (remove-accessible! grid)))
    (if (zero? num-removed) num-total
        (remove-all-accessible! grid (+ num-total num-removed)))))

(define (part-2)
  (display (remove-all-accessible! (read-input) 0))
  (newline))

(main part-1 part-2)
