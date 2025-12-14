#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent main))

(define X 0)
(define Y 1)

(define offset vector)
(define point vector)
(define offset-ref vector-ref)
(define point-ref vector-ref)

(define (string->point s)
  (let ((fields (string-tokenize s char-set:digit)))
    (if (= (length fields) 2)
        (vector (string->number (car fields))
                (string->number (cadr fields)))
        (error "invalid string" s))))

(define (point-add p u)
  (point (+ (point-ref p X) (offset-ref u X))
         (+ (point-ref p Y) (offset-ref u Y))))

(define (point-sub p q)
  (offset (- (point-ref p X) (point-ref q X))
          (- (point-ref p Y) (point-ref q Y))))

(define (offset-abs u)
  (offset (abs (offset-ref u X))
          (abs (offset-ref u Y))))

(define offset-add point-add)
(define offset-sub point-sub)

;; Signed area of a rectangle defined by the offset vector.
(define (offset-area u)
  (* (offset-ref u X)
     (offset-ref u Y)))

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
