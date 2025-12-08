#!/usr/bin/env gosh

(import (scheme base)
        (scheme cxr)
        (scheme write)
        (srfi 28)
        (srfi 43)
        (srfi 117)
        (advent input)
        (advent grid)
        (advent main))

;; ---------------------------------------------------------------------------
;; Part 1

(define (beam-splitting! grid)
  (grid-fold
   (lambda (n m beam this)
     (let ((up (grid-ref grid (- n 1) m))
           (left (grid-ref grid n (- m 1)))
           (right (grid-ref grid n (+ m 1))))
       (cond
        ((eqv? this #\S)
         (grid-set! grid n m beam)
         (+ beam 1))

        ((and (eqv? this #\.)
              (number? up))
         (grid-set! grid n m up)
         beam)

        ((and (eqv? this #\.)
              (eqv? left #\^)           ; XXX
              (number? (grid-ref grid (- n 1) (- m 1))))
         (grid-set! grid n m beam)
         (+ beam 1))

        ((and (eqv? this #\.)
              (eqv? left #\>))
         (grid-set! grid n m (- beam 1))
         beam)

        ((and (eqv? this #\.)
              (eqv? right #\^)
              (number? (grid-ref grid (- n 1) (+ m 1))))
         (grid-set! grid n m beam)
         (grid-set! grid n (+ m 1) #\>)
         (+ beam 1))

        (else beam))))
   1 grid))

(define (beam-splitting2! grid)
  ;; Enqueue beginning of the beam 0
  (let ((start (vector-index
                (lambda (c)
                  (char=? c #\S))
                (vector-ref grid 0))))
    (do ((queue (list-queue `(0 ,start 0)))
         (next-beam 1))
        ((list-queue-empty? queue) next-beam)
      (let* ((state (list-queue-remove-front! queue))
             (n (car state))
             (m (cadr state))
             (beam (caddr state))
             (enqueued #f))

        (display state (current-error-port)) (newline (current-error-port))

        (case (grid-ref grid n m)
          ((#\. #\S)
           (grid-set! grid n m beam)
           (list-queue-add-front! queue `(,(+ n 1) ,m ,beam)))
          ((#\^)
           (when (eqv? (grid-ref grid n (- m 1)) #\.)
             (list-queue-add-back! queue `(,n ,(- m 1) ,next-beam))
             (set! enqueued #t))
           (when (eqv? (grid-ref grid n (+ m 1)) #\.)
             (list-queue-add-back! queue `(,n ,(+ m 1) ,next-beam))
             (set! enqueued #t))
           (when enqueued
             (set! next-beam (+ next-beam 1)))))))))

(define (part-1)
  (let ((grid (lines->grid (read-lines))))
    (display (beam-splitting2! grid))
    (newline)

    (let ((output (current-error-port)))
      (vector-for-each
       (lambda (n row)
         (vector-for-each
          (lambda (m elt)
            (display #\space output)
            (cond
             ((char? elt)
              (display elt output)
              (display elt output))
             ((and (number? elt) (< elt 10))
              (display 0 output)
              (display elt output))
             ((number? elt)
              (display elt output))))
          row)
         (newline output))
       grid)
      (newline))

    ))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
