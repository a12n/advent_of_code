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

;; ---------------------------------------------------------------------------
;; Junction boxes

(define (string->junction s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\,)))))
    (if (= (length fields) 3)
        (vector (string->number (car fields))
                (string->number (cadr fields))
                (string->number (caddr fields)))
        (error "invalid string" s))))

(define (read-input)
  (list->vector (map string->junction (read-lines))))

(define (squared-distance a b)
  (+ (square (- (vector-ref a 0) (vector-ref b 0)))
     (square (- (vector-ref a 1) (vector-ref b 1)))
     (square (- (vector-ref a 2) (vector-ref b 2)))))

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

;; ---------------------------------------------------------------------------
;; Connections between junctions

;; Connection is a cons cell with junction indices in car and cdr.
(define conn-from car)
(define conn-to cdr)

;; Apply function to all possible connections between N elements.
;; f (0 . 1)
;; f (0 . 2)
;; f (0 . 3)
;; …
;; f (0 . N - 1)
;; f (1 . 2)
;; f (1 . 3)
;; …
;; f (N - 2 . N - 1)
(define (fold-conn-combinations f acc n)
  (let loop ((acc acc)
             (i 0)
             (j 1))
    (cond
     ((= i (- n 1)) acc)
     ((= j n) (loop acc (+ i 1) (+ i 2)))
     (else (loop (f (cons i j) acc) i (+ j 1))))))

;; List of all connection combinations between N elements. No need to
;; reverse since the list will be sorted anyway.
(define (conn-combinations-reverse n)
  (fold-conn-combinations cons '() n))

;; Sort list of connections by squared distance between connection
;; endpoints.
(define (conn-list-sort junctions conns)
  (map car
       (list-sort (lambda (a b)
                    (< (cdr a)
                       (cdr b)))
                  (map (lambda (c)
                         ;; Attach squared distance to
                         ;; each endpoint, and strip it
                         ;; after sorting.
                         (cons c (squared-distance (vector-ref junctions (conn-from c))
                                                   (vector-ref junctions (conn-to c)))))
                       conns))))

;; ---------------------------------------------------------------------------
;; Disjoint sets

(define (make-disjoint-set-parents n)
  (vector-unfold values n))

(define (make-disjoint-set-sizes n)
  (make-vector n 1))

(define (disjoint-set-find! parents i)
  (let ((j (vector-ref parents i)))
    (if (= j i) i
        (let ((u (disjoint-set-find! parents j)))
          (vector-set! parents i u)
          u))))

(define (disjoint-set-union! parents sizes i j)
  (let ((u (disjoint-set-find! parents i))
        (v (disjoint-set-find! parents j)))
    (if (= u v) u
        (let ((n (vector-ref sizes u))
              (m (vector-ref sizes v)))
          (cond
           ((> n m)
            (vector-set! parents v u)
            (vector-set! sizes u (+ n m))
            u)
           (else
            (vector-set! parents u v)
            (vector-set! sizes v (+ n m))
            v))))))

;; ---------------------------------------------------------------------------
;; Part 1

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

;; TODO: Rewrite with disjoint sets.
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

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  (let* ((junctions (read-input))
         (n (vector-length junctions))
         ;; Parent links and sizes of disjoint sets
         (parents (make-disjoint-set-parents n))
         (sizes (make-disjoint-set-sizes n))
         ;; Find first junction connection, such that…
         (last-conn
          (find (lambda (c)
                  ;; …after merging sets of connections for its endpoints,
                  ;; there's now single connected component of size N, …
                  (let ((u (disjoint-set-union! parents sizes (conn-from c) (conn-to c))))
                    (= (vector-ref sizes u) n)))
                ;; …in the list of all combinations of connections, sorted
                ;; by squared distance between connection endpoints.
                (conn-list-sort junctions (conn-combinations-reverse n))))
         ;; Extract endpoints of the last connection.
         (p (vector-ref junctions (conn-from last-conn)))
         (q (vector-ref junctions (conn-to last-conn))))
    ;; Print answer.
    (display (* (vector-ref p 0)
                (vector-ref q 0)))
    (newline)))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
