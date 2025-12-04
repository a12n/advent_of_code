#!/usr/bin/env gsi-script

(define (read-lines #!optional (input (current-input-port)))
  (let ((line (read-line input)))
    (if (eof-object? line) '()
        (cons line (read-lines input)))))

(define (lines->grid lines)
  (list->vector (map string->vector lines)))

(define (grid-size grid)
  (let ((h (vector-length grid)))
    (if (zero? h) (values 0 0)
        (values (vector-length (vector-ref grid 0)) h))))

(define (grid-ref grid x y #!optional default)
  (if (and (>= y 0) (< y (vector-length grid)))
      (let ((row (vector-ref grid y)))
        (if (and (>= x 0) (< x (vector-length row)))
            (vector-ref row x)
            default))
      default))

(define (grid-ref* grid x y)
  (vector-ref (vector-ref grid y) x))

(define (fold-positions f acc w h)
  (define (fold-row acc y)
    (define (loop-x x acc)
      (if (>= x w) acc
          (loop-x (+ x 1) (f x y acc))))
    (loop-x 0 acc))
  (define (loop-y y acc)
    (if (>= y h) acc
        (loop-y (+ y 1) (fold-row acc y))))
  (loop-y 0 acc))

(define (fold-neighbors-8 f acc)
  (let* ((acc (f -1 -1 acc))
         (acc (f  0 -1 acc))
         (acc (f +1 -1 acc))
         (acc (f -1  0 acc))
         (acc (f +1  0 acc))
         (acc (f -1 +1 acc))
         (acc (f  0 +1 acc))
         (acc (f +1 +1 acc)))
    acc))

(define (num-neighbors grid x y)
  (fold-neighbors-8
   (lambda (dx dy n)
     (+ n (if (char=? (grid-ref grid (+ x dx) (+ y dy) #\.) #\@) 1 0)))
   0))

(let ((grid (lines->grid (read-lines))))
  (receive (w h) (grid-size grid)
    (let ((n (fold-positions
              (lambda (x y n)
                (+ n (if (and (char=? (grid-ref* grid x y) #\@) (< (num-neighbors grid x y) 4)) 1 0)))
              0 w h)))
      (display n)
      (newline))))
