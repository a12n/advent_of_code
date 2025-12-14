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

(define (overlapping-segments? s t)
  (and (overlapping-intervals? (segment-projection s X)
                               (segment-projection t X))
       (overlapping-intervals? (segment-projection s Y)
                               (segment-projection t Y))))

(define (part-2)
  (let* ((points (read-input))
         (outline-segments (points->segments (integer-polygon-offset 'ccw points))))

        (display (list "points" (length points) points) (current-error-port))
        (newline (current-error-port))

        (display (list "outline-segments" (length outline-segments) outline-segments) (current-error-port))
        (newline (current-error-port))

        (display
         (fold-combinations
          (lambda (comb area)
            (let* ((right-bottom (car comb))
                   (left-top (cdr comb))
                   (left-bottom (point (point-x left-top)
                                       (point-y right-bottom)))
                   (right-top (point (point-x right-bottom)
                                     (point-y left-top))))
              (if (any (lambda (s)
                         (or (overlapping-segments? (cons left-top right-top) s)
                             (overlapping-segments? (cons left-top left-bottom) s)
                             (overlapping-segments? (cons right-top right-bottom) s)
                             (overlapping-segments? (cons left-bottom right-bottom) s)))
                       outline-segments)
                  area
                  (max area (rectangle-area left-top right-bottom)))))
          0 points))
        (newline)

    ))

  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
