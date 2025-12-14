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
         (segments (points->segments points))
         ;; XXX: Find the outliers automatically (e.g., compare with
         ;; mean segment length)?
         (p (point 94651 48450))
         (q (point 94651 50319)))

    ;; Remove segments which have an endpoint below the outlier point
    ;; Q. They can't form a valid rectange with Q.
    (set! segments
          (remove
           (lambda (s)
             (or (< (point-ref (interval-lower s) Y)
                    (point-ref q Y))
                 (< (point-ref (interval-upper s) Y)
                    (point-ref q Y))))
           segments))

    ;; Find horizontal segment which intersects with x=Qx line. Remove
    ;; segments above this found one. Points of these removed segments
    ;; couldn't form a valid rectange with Q.
    (let ((r (segment-begin
              (find
               (lambda (s)
                 (and (segment-horiz? s)
                      (> (point-ref (segment-begin s) Y) (point-ref q Y))
                      (interval-member? (point-ref q X)
                                        (make-interval (point-ref (segment-begin s) X)
                                                       (point-ref (segment-end s) X)))))
               segments))))
      (set! segments
            (remove
             (lambda (s)
               (or (> (point-ref (segment-begin s) Y)
                      (point-ref r Y))
                   (> (point-ref (segment-end s) Y)
                      (point-ref r Y))))
             segments)))

    ;; Remove segments to the right of Q. Segments to the left are
    ;; clearly better.
    (set! segments
          (remove
           (lambda (s)
             (or (> (point-ref (segment-begin s) X)
                    (point-ref q X))
                 (> (point-ref (segment-end s) X)
                    (point-ref q X))))
           segments))

    ;; Print segments.
    (for-each
     (lambda (s)
       (display (vector-ref (car s) 0))
       (display ",")
       (display (vector-ref (car s) 1))
       (display " ")
       (display (vector-ref (cdr s) 0))
       (display ",")
       (display (vector-ref (cdr s) 1))
       (display "\n"))
     segments)

  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
