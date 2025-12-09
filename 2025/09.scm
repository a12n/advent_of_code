#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent main))

(define (string->position s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\,)))))
    (if (= (length fields) 2)
        (vector (string->number (car fields))
                (string->number (cadr fields)))
        (error "invalid string" s))))

(define (position-add p u)
  (vector (+ (vector-ref p 0)
             (vector-ref u 0))
          (+ (vector-ref p 1)
             (vector-ref u 1))))

(define (position-sub p q)
  (vector (- (vector-ref p 0)
             (vector-ref q 0))
          (- (vector-ref p 1)
             (vector-ref q 1))))

;; Signed area of a rectangle defined by the offset vector.
(define (offset-area u)
  (* (vector-ref u 0)
     (vector-ref u 1)))

(define (read-input)
  (fold-lines (lambda (line positions)
                (cons (string->position line)
                      positions))
              '()))

;; ---------------------------------------------------------------------------
;; Part 1

(define (fold-combinations f acc list)
  (if (null? list) acc
      (fold-combinations
       f
       (fold (lambda (b acc) (f (cons (car list) b) acc)) acc (cdr list))
       (cdr list))))

(define (part-1)
  (let ((positions (read-input)))
    (display
     (fold-combinations
      (lambda (pair area)
        (let ((p (car pair))
              (q (cdr pair)))
          (max area (abs (offset-area (position-sub p (position-add q #(1 1))))))))
      0 positions))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
