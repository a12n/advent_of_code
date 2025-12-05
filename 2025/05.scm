#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent segment)
        (advent main))

(define (read-input)
  (let ((lines (read-lines)))
    (let-values (((interval-lines ingredient-lines) (break string-null? lines)))
      (let ((intervals (map string->segment interval-lines))
            (ingredients (map string->number (cdr ingredient-lines))))
        (values (make-segment-set intervals) ingredients)))))

(define (part-1)
  (let-values (((database ingredients) (read-input)))
    (let ((fresh-ingredients
           (filter
            (lambda (ingredient)
              (in-segment-set? ingredient database))
            ingredients)))
      (display (length fresh-ingredients))
      (newline))))

(define (part-2)
  (let-values (((database _) (read-input)))
    (display (segment-set-size database))
    (newline)))

(main part-1 part-2)
