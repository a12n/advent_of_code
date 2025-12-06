#!/usr/bin/env gosh

(import (scheme base)
        (scheme char)
        (scheme eval)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (advent input)
        (advent grid)
        (advent main))

;; ---------------------------------------------------------------------------
;; Part 1

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

;; ---------------------------------------------------------------------------
;; Part 2

;; FIXME
(define (part-2)
  (let* ((worksheet (lines->grid (read-lines)))
         (n (grid-rows worksheet))
         (m (grid-cols worksheet)))
    (display
     (let loop ((total 0)
                (numbers '())
                (number 0)
                (col (- m 1))
                (row 0))
       (cond
        ;; Exit condition
        ((< col 0) total)
        ;; Done one column
        ((= row n)
         (loop total
               numbers
               0
               (- col 1)
               0))
        ;; Found digit
        ((char-numeric? (grid-ref worksheet row col))
         (loop total
               numbers
               (+ (* number 10) (digit-value (grid-ref worksheet row col)))
               col
               (+ row 1)))
        ;; Found space
        ((char-whitespace? (grid-ref worksheet row col))
         (loop total
               (if (zero? number) numbers (cons number numbers))
               0
               col
               (+ row 1)))
        ;; Found multiplication
        ((char=? (grid-ref worksheet row col) #\*)
         (loop (+ total (apply * (if (zero? number) numbers (cons number numbers))))
               '()
               0
               col
               (+ row 1)))
        ;; Found addition
        ((char=? (grid-ref worksheet row col) #\+)
         (loop (+ total (apply + (if (zero? number) numbers (cons number numbers))))
               '()
               0
               col
               (+ row 1))))
       ))
    (newline)))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
