#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent main))

(define (read-input)
  (fold-lines
   (lambda (line alist)
     (let ((fields (string-tokenize line char-set:lower-case)))
       (if (< (length fields) 2)
           (error "invalid line" line))
       (let* ((symbols (map string->symbol fields))
              (source (car symbols))
              (dests (cdr symbols)))
         (alist-cons source dests alist))))
   '()))

;; ---------------------------------------------------------------------------
;; Part 1

(define (part-1)
  (let ((graph (read-input)))
    (display graph)
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  (display 0)
  (newline))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
