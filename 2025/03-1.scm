#!/usr/bin/env gsi-script

(include "03.scm")

(define (largest-joltage-index b)
  (let ((n (vector-length b)))
    (define (index-max start end index)
      (if (= start end) index
          (index-max (+ start 1) end
                     (if (or (negative? index)
                             (> (vector-ref b start)
                                (vector-ref b index)))
                         start index))))
    (let* ((i (index-max 0 n -1))
           (j (index-max (+ i 1) n -1)))
      (if (negative? j)
          (values (index-max 0 i -1) i)
          (values i j)))))

(define (largest-joltage b)
  (receive (i j)
      (largest-joltage-index b)
    (+ (* (vector-ref b i) 10)
       (vector-ref b j))))

(define (total-output-joltage sum)
  (let ((line (read-line)))
    (if (eof-object? line) sum
        (total-output-joltage
         (+ sum (largest-joltage (string->battery-bank line)))))))

(display (total-output-joltage 0))
(newline)
