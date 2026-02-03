(define-library (advent grid)
  (export lines->grid grid-copy grid-unfold
          grid-rows grid-cols
          grid-ref grid-set!
          grid-fold grid-map! grid-for-each
          grid-index)

  (export fold-grid-neighbors-4
          fold-grid-neighbors-8)

  (import (scheme base)
          (srfi 43))

  (begin

    (define (lines->grid lines)
      (list->vector (map string->vector lines)))

    (define (grid-copy grid)
      (vector-map
       (lambda (n row)
         (vector-copy row))
       grid))

    (define (grid-unfold f rows cols)
      (vector-unfold
       (lambda (n)
         (vector-unfold
          (lambda (m)
            (f n m))
          cols))
       rows))

    (define (grid-rows grid)
      (vector-length grid))

    (define (grid-cols grid)
      (vector-length (vector-ref grid 0)))

    (define (grid-ref grid n m)
      (and (< -1 n (grid-rows grid))
           (< -1 m (grid-cols grid))
           (vector-ref (vector-ref grid n) m)))

    (define (grid-set! grid n m value)
      (vector-set! (vector-ref grid n) m value))

    (define (grid-fold f acc grid)
      (vector-fold (lambda (n acc row)
                     (vector-fold (lambda (m acc elt)
                                    (f n m acc elt))
                                  acc row))
                   acc grid))

    (define (grid-map! f grid)
      (vector-map!
       (lambda (n row)
         (vector-map!
          (lambda (m elt)
            (f n m elt))
          row)
         row)
       grid))

    (define (grid-for-each f grid)
      (vector-for-each
       (lambda (n row)
         (vector-for-each
          (lambda (m elt)
            (f n m elt))
          row))
       grid))

    (define (grid-index pred grid)
      (let ((rows (grid-rows grid))
            (cols (grid-cols grid)))
        (let loop ((n 0)
                   (m 0))
          (cond
           ((= n rows) (values #f #f))
           ((= m cols) (loop (+ n 1) 0))
           ((pred (vector-ref (vector-ref grid n) m)) (values n m))
           (else (loop n (+ m 1)))))))

    (define (fold-grid-neighbors-4 f acc)
      (let* ((acc (f -1  0 acc))
             (acc (f  0 -1 acc))
             (acc (f  0 +1 acc))
             (acc (f +1  0 acc)))
        acc))

    (define (fold-grid-neighbors-8 f acc)
      (let* ((acc (f -1 -1 acc))
             (acc (f -1  0 acc))
             (acc (f -1 +1 acc))
             (acc (f  0 -1 acc))
             (acc (f  0 +1 acc))
             (acc (f +1 -1 acc))
             (acc (f +1  0 acc))
             (acc (f +1 +1 acc)))
        acc))

    ))
