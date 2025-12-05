(define-library (advent segment)
  (export in-segment? segment-length segments-union string->segment)
  (export in-segment-set? make-segment-set segment-set-size)

  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 13)
          (srfi 14)
          (srfi 132))

  (begin
    ;; Single integer segment

    (define (segment< s t)
      (< (cdr s) (car t)))

    (define (disjoint-segments? s t)
      (or (segment< s t)
          (segment< t s)))

    (define (empty-segment? s)
      (> (car s) (cdr s)))

    (define (overlapping-segments? s t)
      (not (disjoint-segments? s t)))

    (define (adjacent-segments? s t)
      (= (+ (cdr s) 1) (car t)))

    (define (segment-length s)
      (if (empty-segment? s) 0 (+ (- (cdr s) (car s)) 1)))

    (define (in-segment? n s)
      (and (>= n (car s))
           (<= n (cdr s))))

    (define (make-segment a b)
      (if (and (integer? a)
               (integer? b))
          (cons (min a b) (max a b))
          (error "invalid numbers" a b)))

    (define (string->segment s)
      (let ((fields (string-tokenize s (char-set-complement (char-set #\-)))))
        (if (= (length fields) 2)
            (make-segment (string->number (car fields))
                          (string->number (cadr fields)))
            (error "invalid string" s))))

    (define (segments-union s t)
      (cond
       ((empty-segment? s) t)
       ((empty-segment? t) s)
       ((or (overlapping-segments? s t)
            (adjacent-segments? s t)
            (adjacent-segments? t s))
        (cons (min (car s) (car t))
              (max (cdr s) (cdr t))))
       (else #f)))

    ;; Sets of integer segments

    (define (make-segment-set segment-list)
      (reverse
       (fold
        (lambda (s segment-set)
          (if (null? segment-set)
              (cons s segment-set)
              (let ((u (segments-union s (car segment-set))))
                (if u
                    (cons u (cdr segment-set))
                    (cons s segment-set)))))
        '()
        (list-sort
         (lambda (s t)
           (< (car s) (car t)))
         segment-list))))

    (define (in-segment-set? n list)
      (any (lambda (s) (in-segment? n s)) list))

    (define (segment-set-size list)
      (fold + 0 (map segment-length list)))

    ))
