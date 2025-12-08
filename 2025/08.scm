#!/usr/bin/env gosh

(import (scheme base)
        (scheme cxr)
        (scheme write)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent main))

(define (string->junction-box s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\,)))))
    (if (= (length fields) 3)
        (vector (string->number (car fields))
                (string->number (cadr fields))
                (string->number (caddr fields)))
        (error "invalid string" s))))

(define (squared-distance p q)
  (+ (square (- (vector-ref p 0) (vector-ref q 0)))
     (square (- (vector-ref p 1) (vector-ref q 1)))
     (square (- (vector-ref p 2) (vector-ref q 2)))))

(define (read-input)
  (list->vector (map string->junction-box (read-lines))))

(define (part-1)
  (let ((junctions (read-input)))
    (display junctions)
    (newline)))

(define (part-2)
  ;; TODO
  )

(main part-1 part-2)
