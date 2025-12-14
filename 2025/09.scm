#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent main))

(define (string->point s)
  (let ((fields (string-tokenize s char-set:digit)))
    (if (= (length fields) 2)
        (vector (string->number (car fields))
                (string->number (cadr fields)))
        (error "invalid string" s))))

(define (point-x p)
  (vector-ref p 0))

(define (point-y p)
  (vector-ref p 1))

(define offset-x point-x)
(define offset-y point-y)

(define (point-add p u)
  (vector (+ (point-x p) (offset-x u))
          (+ (point-y p) (offset-y u))))

(define (point-sub p q)
  (vector (- (point-x p) (point-x q))
          (- (point-y p) (point-y q))))

(define (offset-abs u)
  (vector (abs (offset-x u))
          (abs (offset-y u))))

(define offset-add point-add)
(define offset-sub point-sub)

;; Signed area of a rectangle defined by the offset vector.
(define (offset-area u)
  (* (offset-x u) (offset-y u)))

(define (read-input)
  (reverse
   (fold-lines (lambda (line points)
                 (cons (string->point line)
                       points))
               '())))

;; ---------------------------------------------------------------------------
;; Part 1

(define (fold-combinations f acc list)
  (if (null? list) acc
      (fold-combinations
       f
       (fold (lambda (b acc) (f (cons (car list) b) acc)) acc (cdr list))
       (cdr list))))

(define (part-1)
  (let ((points (read-input)))
    (display
     (fold-combinations
      (lambda (pair area)
        (let ((p (car pair))
              (q (cdr pair)))
          (max area (offset-area (offset-add (offset-abs (point-sub p q)) #(1 1))))))
      0 points))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
