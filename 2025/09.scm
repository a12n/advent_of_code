#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (advent input)
        (advent interval)
        (advent list)
        (advent main))

(define X 0)
(define Y 1)

(define offset vector)
(define point vector)
(define offset-ref vector-ref)
(define point-ref vector-ref)

(define (string->point s)
  (let ((fields (string-tokenize s char-set:digit)))
    (if (= (length fields) 2)
        (vector (string->number (car fields))
                (string->number (cadr fields)))
        (error "invalid string" s))))

(define (point-add p u)
  (point (+ (point-ref p X) (offset-ref u X))
         (+ (point-ref p Y) (offset-ref u Y))))

(define (point-sub p q)
  (offset (- (point-ref p X) (point-ref q X))
          (- (point-ref p Y) (point-ref q Y))))

(define (offset-abs u)
  (offset (abs (offset-ref u X))
          (abs (offset-ref u Y))))

(define offset-add point-add)
(define offset-sub point-sub)

;; Signed area of a rectangle defined by the offset vector.
(define (offset-area u)
  (* (offset-ref u X)
     (offset-ref u Y)))

(define (rectangle-area p q)
  (offset-area (offset-add (offset-abs (point-sub p q)) #(1 1))))

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

;; Closed line segment endpoints.
(define segment-begin car)
(define segment-end cdr)

(define (segment-horiz? s)
  (= (point-ref (segment-begin s) Y)
     (point-ref (segment-end s) Y)))

(define (segment-vert? s)
  (= (point-ref (segment-begin s) X)
     (point-ref (segment-end s) X)))

(define (segment-projection s dim)
  (make-interval (point-ref (segment-begin s) dim)
                 (point-ref (segment-end s) dim)))

(define (overlapping-segments? s t)
  (and (overlapping-intervals? (segment-projection s X)
                               (segment-projection t X))
       (overlapping-intervals? (segment-projection s Y)
                               (segment-projection t Y))))

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
