(define-library (advent geometry)
  (export X Y
          point string->point
          point-ref point-x point-y
          point-map point-add point-sub
          vector-x vector-y
          vector-map vector-add vector-sub vector-div
          integer-vector-normalize
          vector-rotate)

  (export segment-begin segment-end
          segment-horiz? segment-vert?
          points->segments
          fold-polygon integer-polygon-offset)

  (import (scheme base)
          (srfi 13)
          (srfi 14)
          (srfi 133))

  (begin

    (define X 0)
    (define Y 1)

    (define point vector)
    (define point-map vector-map)
    (define point-ref vector-ref)

    (define (string->point s)
      (let ((fields (string-tokenize s char-set:digit)))
        (if (= (length fields) 2)
            (vector (string->number (car fields))
                    (string->number (cadr fields)))
            (error "invalid string" s))))

    (define (vector-x u) (vector-ref u X))
    (define (vector-y u) (vector-ref u Y))
    (define point-x vector-x)
    (define point-y vector-y)

    (define (vector-add u v) (vector-map + u v))
    (define (vector-sub u v) (vector-map - u v))
    (define point-add vector-add)
    (define point-sub vector-sub)

    (define (vector-div u n)
      (vector-map (lambda (ui) (/ ui n)) u))

    (define (integer-vector-normalize u)
      (vector-div u (gcd (vector-x u)
                         (vector-y u))))

    (define (vector-rotate dir u)
      (case dir
        (('cw)
         (vector (- (vector-y u))
                 (vector-x u)))
        (('ccw)
         (vector (vector-y u)
                 (- (vector-x u))))))

    ;; Closed line segment, a cons pair of endpoints.
    (define segment-begin car)
    (define segment-end cdr)

    (define (segment-horiz? s)
      (= (point-y (segment-begin s))
         (point-y (segment-end s))))

    (define (segment-vert? s)
      (= (point-x (segment-begin s))
         (point-x (segment-end s))))

    (define (points->segments points)
      (define (loop points-left)
        (cond
         ;; Last point, make segment with the first point.
         ((null? (cdr points-left))
          (list (cons (car points-left)
                      (car points))))
         (else
          (cons (cons (car points-left)
                      (cadr points-left))
                (loop (cdr points-left))))))
      (cond
       ((null? points) '())
       ((null? (cdr points)) '())
       (else (loop points))))

    ;; points — polygonal chain of the polygon points.
    (define (fold-polygon f acc points)
      (define (loop prev points-left acc)
        (if (null? (cdr points-left))
            (f prev
               (car points-left)
               (car points)
               acc)
            (loop (car points-left)
                  (cdr points-left)
                  (f prev
                     (car points-left)
                     (cadr points-left)
                     acc))))
      (loop (last points) points acc))

    ;; dir — either 'cw or 'ccw.
    ;; points — polygonal chain of the simple polygon points (integer
    ;; coordinates).
    (define (integer-polygon-offset dir points)
      (reverse
       (fold-polygon
        (lambda (prev this next points)
          (let ((normal-prev (vector-rotate dir (integer-vector-normalize (point-sub this prev))))
                (normal-next (vector-rotate dir (integer-vector-normalize (point-sub next this)))))
            (cons (point-add this
                             (vector-add normal-prev
                                         normal-next))
                  points)))
        '()
        points)))

    ))
