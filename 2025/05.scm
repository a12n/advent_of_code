#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent main))

(define (read-input)
  (let ((lines (read-lines)))
    (let-values (((interval-lines ingredient-lines) (break string-null? lines)))
      (values (map string->number-range interval-lines)
              (map string->number (cdr ingredient-lines))))))

(define (part-1)
  (let-values (((intervals ingredients) (read-input)))
    (let ((fresh-ingredients
           (filter
            (lambda (ingredient)
              (any (lambda (interval)
                     (and (>= ingredient (car interval))
                          (<= ingredient (cdr interval))))
                   intervals))
            ingredients)))
      (display (length fresh-ingredients))
      (newline))))

(main part-1)
