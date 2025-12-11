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

(define (graphviz graph)
  (display "digraph {\n")
  (for-each
   (lambda (source-dests)
     (for-each
      (lambda (dest)
        (display "	")
        (display (car source-dests))
        (display " -> ")
        (display dest)
        (display ";\n"))
      (cdr source-dests)))
   graph)
  (display "}\n"))

;; ---------------------------------------------------------------------------
;; Part 1

(define (num-paths graph source dest)
  (define (loop path)
    (if (eq? (car path) dest) 1
        (fold
         + 0
         (map
          (lambda (v)
            (loop (cons v path)))
          (remove
           (lambda (v)
             (memq v path))
           (assq (car path) graph))))))
  (loop (list source)))

(define (part-1)
  (let ((graph (read-input)))
    (display (num-paths graph 'you 'out))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  (display 0)
  (newline))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
