(define-library (advent interval)
  (export make-interval string->interval
          interval-lower interval-upper
          interval-member? interval-size integer-interval-size
          intervals-union)

  (export make-interval-set
          interval-set-member? interval-set-size integer-interval-set-size)

  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 13)
          (srfi 14)
          (srfi 132))

  (begin

    (define interval-lower car)
    (define interval-upper cdr)

    ;; Single closed interval

    (define (interval< s t)
      (< (interval-upper s) (interval-lower t)))

    (define (disjoint-intervals? s t)
      (or (interval< s t) (interval< t s)))

    (define (empty-interval? s)
      (> (interval-lower s) (interval-upper s)))

    (define (overlapping-intervals? s t)
      (not (disjoint-intervals? s t)))

    (define (adjacent-integer-intervals? s t)
      (= (+ (interval-upper s) 1) (interval-lower t)))

    (define (interval-size s)
      (if (empty-interval? s) 0
          (- (interval-upper s)
             (interval-lower s))))

    (define (integer-interval-size s)
      (if (empty-interval? s) 0
          (+ (- (interval-upper s)
                (interval-lower s))
             1)))

    (define (interval-member? x s)
      (and (>= x (interval-lower s))
           (<= x (interval-upper s))))

    (define (make-interval a b)
      (if (< a b)
          (cons a b)
          (cons b a)))

    (define (string->interval s)
      (let ((fields (string-tokenize s char-set:digit)))
        (if (= (length fields) 2)
            (make-interval (string->number (car fields))
                           (string->number (cadr fields)))
            (error "invalid string" s))))

    (define (intervals-union s t)
      (cond
       ((empty-interval? s) t)
       ((empty-interval? t) s)
       ((or (overlapping-intervals? s t)
            (adjacent-integer-intervals? s t)
            (adjacent-integer-intervals? t s))
        (cons (min (interval-lower s) (interval-lower t))
              (max (interval-upper s) (interval-upper t))))
       (else #f)))

    ;; Sets of closed intervals

    (define (make-interval-set intervals)
      (reverse
       (fold
        (lambda (s intervals)
          (if (null? intervals)
              (cons s intervals)
              (let ((u (intervals-union s (car intervals))))
                (if u
                    (cons u (cdr intervals))
                    (cons s intervals)))))
        '()
        (list-sort
         (lambda (s t)
           (< (interval-lower s)
              (interval-lower t)))
         intervals))))

    (define (interval-set-member? x intervals)
      (any (lambda (s) (interval-member? x s)) intervals))

    (define (interval-set-size intervals)
      (fold + 0 (map interval-size intervals)))

    (define (integer-interval-set-size intervals)
      (fold + 0 (map integer-interval-size intervals)))

    ))
