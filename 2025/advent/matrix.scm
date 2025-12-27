(define-library (advent matrix)
  (export matrix-rows matrix-cols
          matrix-unfold matrix-copy make-matrix make-matrix-identity
          matrix-ref matrix-ref-row matrix-ref-col
          matrix-set! matrix-set-row! matrix-set-col!
          matrix-fold
          matrix-map! matrix-map-row! matrix-map-col!)

  (import (scheme base)
          (scheme case-lambda)
          (srfi 43))

  (begin

    (define (matrix-rows a)
      (vector-length a))

    (define (matrix-cols a)
      (if (vector-empty? a) 0
          (vector-length (vector-ref a 0))))

    (define (matrix-unfold f n m)
      (vector-unfold
       (lambda (i)
         (vector-unfold
          (lambda (j)
            (f i j))
          m))
       n))

    (define matrix-copy
      (case-lambda
       ((a)
        (let ((b (vector-copy a)))
          (vector-map!
           (lambda (_ u)
             (vector-copy u))
           b)
          b))
       ((a rstart rend cstart cend)
        (let ((b (vector-copy a rstart rend)))
          (vector-map!
           (lambda (_ u)
             (vector-copy u cstart cend))
           b)
          b))))

    (define (make-matrix n m x)
      (matrix-unfold (lambda (i j) x) n m))

    (define (make-matrix-identity n)
      (matrix-unfold (lambda (i j) (if (= i j) 1 0)) n n))

    (define (matrix-ref a i j)
      (vector-ref (vector-ref a i) j))

    (define (matrix-ref-row a i)
      (vector-ref a i))

    (define (matrix-ref-col a j)
      (vector-unfold
       (lambda (i)
         (matrix-ref a i j))
       (vector-length a)))

    (define (matrix-set! a i j x)
      (vector-set! (vector-ref a i) j x))

    (define (matrix-set-row! a i u)
      (if (= (vector-length (vector-ref a i))
             (vector-length u))
          (vector-set! a i u)
          (error "invalid length" u)))

    (define (matrix-set-col! a j v)
      (if (= (vector-length a)
             (vector-length v))
          (vector-for-each
           (lambda (i x)
             (matrix-set! a i j x))
           v)
          (error "invalid length" v)))

    (define matrix-fold
      (case-lambda
       ((f acc a)
        (matrix-fold f acc a
                     0 (matrix-rows a)
                     0 (matrix-cols a)))
       ((f acc a rstart rend cstart cend)
        (let loop ((i rstart)
                   (j cstart)
                   (acc acc))
          (cond
           ((= i rend) acc)
           ((= j cend) (loop (+ i 1) rstart acc))
           (else (loop i (+ j 1) (f i j acc (matrix-ref a i j)))))))))

    (define matrix-map!
      (case-lambda
       ((f a)
        (matrix-map! f a
                     0 (matrix-rows a)
                     0 (matrix-cols a)))
       ((f a rstart rend cstart cend)
        (matrix-fold
         (lambda (i j _ x)
           (matrix-set! a i j (f i j x)))
         #f a rstart rend cstart cend))))

    (define matrix-map-row!
      (case-lambda
       ((f a i)
        (matrix-map-row! f a i 0 (matrix-cols a)))
       ((f a i cstart cend)
        (matrix-fold
         (lambda (_ j _ x)
           (f j x))
         #f a i (+ i 1) cstart cend))))

    (define matrix-map-col!
      (case-lambda
       ((f a j)
        (matrix-map-col! f a j 0 (matrix-rows a)))
       ((f a j rstart rend)
        (matrix-fold
         (lambda (i _ _ x)
           (f i x))
         #f a rstart rend j (+ j 1)))))

    ))
