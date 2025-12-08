#!/usr/bin/env gosh

(import (scheme base)
        (scheme cxr)
        (scheme process-context)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (srfi 43)
        (srfi 132)
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

(define (junction-distances junctions)
  (let ((n (vector-length junctions)))
    (let loop ((distances '())
               (i 0)
               (j 1))
      (cond
       ((= i (- n 1)) distances)
       ((= j n) (loop distances (+ i 1) (+ i 2)))
       (else (loop (alist-cons (cons i j)
                               (squared-distance (vector-ref junctions i)
                                                 (vector-ref junctions j))
                               distances)
                   i (+ j 1)))))))

;; Vector of connected component labels for N junctions. Takes
;; adjacency vector as the parameter.
(define (connected-components adjacency)
  (let* ((n (vector-length adjacency))
         (components (make-vector n -1)))
    (define (dfs i id)
      (when (= (vector-ref components i) -1)
        (vector-set! components i id)
        (for-each (lambda (j) (dfs j id))
                  (vector-ref adjacency i))))
    (do ((i 0 (+ i 1)))
        ((= i n) components)
      (dfs i i))))

;; Frequency vector of connected components. Value at index `c` is
;; size of connected component `c`. Takes connected component labels
;; vector as the parameter.
(define (component-lengths components)
  (let* ((n (vector-length components))
         (lengths (make-vector n 0)))
    (vector-for-each (lambda (u c)
                       (vector-set! lengths c (+ (vector-ref lengths c) 1)))
                     components)
    lengths))

;; Make junction adjacency vector for N junctions from list of
;; connection pairs. Each pair is cons of I and J junction indices.
(define (junction-adjacency n conns)
  (let ((adjacency (make-vector n '())))
    (for-each (lambda (conn)
                (let ((i (car conn))
                      (j (cdr conn)))
                  (vector-set! adjacency i (cons j (vector-ref adjacency i)))
                  (vector-set! adjacency j (cons i (vector-ref adjacency j)))))
              conns)
    adjacency))

;; TODO: Explicit heap of size k.
(define (part-1)
  (let* ((junctions (read-input))
         (n (vector-length junctions))
         (k (string->number (or (get-environment-variable "CONNS") "1000"))))
    (let* ((distances (junction-distances junctions))
           (distances-sorted (list-sort (lambda (a b)
                                          (< (cdr a) (cdr b)))
                                        distances))
           (adjacency (junction-adjacency n (map car (take distances-sorted k))))
           (components (connected-components adjacency))
           (lengths (component-lengths components))
           (lengths-sorted (vector-sort > lengths)))

      (display (fold * 1 (vector->list lengths-sorted 0 3)))
      (newline))))

(define (part-2)
  ;; TODO
  )

(main part-1 part-2)
