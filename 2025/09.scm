#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (advent geometry)
        (advent input)
        (advent interval)
        (advent list)
        (advent main))

(define (rectangle-area p q)
  (let ((v (vector-add (vector-map abs (point-sub p q)) #(1 1))))
    (* (vector-x v)
       (vector-y v))))

(define (read-input)
  (reverse
   (fold-lines (lambda (line points)
                 (cons (string->point line)
                       points))
               '())))

;; ---------------------------------------------------------------------------
;; Part 1

(define (part-1)
  (let ((points (read-input)))
    (display
     (fold-combinations
      (lambda (pair area)
        (let ((p (car pair))
              (q (cdr pair)))
          (max area (rectangle-area p q))))
      0 points))
    (newline)))

;; ---------------------------------------------------------------------------
;; Part 2

(define (segment-projection s dim)
  (make-interval (point-ref (segment-begin s) dim)
                 (point-ref (segment-end s) dim)))

(define (part-2)
  (let* ((points (read-input))
         (outline-intervals
          (map (lambda (s)
                 (cons (segment-projection s X)
                       (segment-projection s Y)))
               (points->segments (integer-polygon-offset 'ccw points)))))

        (display
         (fold-combinations
          (lambda (comb area)
            ;; XXX: Build interval trees of horizontal and vertical segments?
            (let* ((p (car comb))
                   (q (cdr comb))
                   ;; PxPy----------QxPy
                   ;;  |             |
                   ;;  |             |
                   ;;  |             |
                   ;; PxQy----------QxQy
                   (px (point-x p))
                   (py (point-y p))
                   (qx (point-x q))
                   (qy (point-y q)))
              (if (any (lambda (s)
                         (or (and (overlapping-intervals? (make-interval px qx) (car s))
                                  (overlapping-intervals? (make-interval py py) (cdr s)))
                             (and (overlapping-intervals? (make-interval px px) (car s))
                                  (overlapping-intervals? (make-interval py qy) (cdr s)))
                             (and (overlapping-intervals? (make-interval qx qx) (car s))
                                  (overlapping-intervals? (make-interval py qy) (cdr s)))
                             (and (overlapping-intervals? (make-interval px qx) (car s))
                                  (overlapping-intervals? (make-interval qy qy) (cdr s)))))
                       outline-intervals)
                  area
                  (max area (rectangle-area p q)))))
          0 points))
        (newline)

    ))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
