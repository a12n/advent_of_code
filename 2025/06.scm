#!/usr/bin/env gosh

(import (scheme base)
        (scheme eval)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (advent input)
        (advent main))

(define (transpose lists)
  (cond
   ((null? lists) '())
   ((null? (car lists)) '())
   (else
    (let ((col (map car lists))
          (cols (map cdr lists)))
      (cons col (transpose cols))))))

(define (read-input)
  (let* ((lines (read-lines))
         (token-lists (reverse (map string-tokenize lines)))
         (value-lists (cons (map string->symbol (car token-lists))
                            (map (lambda (numbers-line)
                                   (map string->number numbers-line))
                                 (cdr token-lists)))))
    (transpose value-lists)))

(define (part-1)
  (let* ((env (environment '(scheme base)))
         (answers (map (lambda (expr)
                         (eval expr env))
                       (read-input))))
    (display (fold + 0 answers))
    (newline)))

(main part-1)
