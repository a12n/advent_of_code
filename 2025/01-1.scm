#!/usr/bin/env gsi-script

(include "aoc.scm")
(include "01.scm")

;; Simple password method (the dial is pointing at 0 after a
;; rotation).
(define (rotate-simple dial rotation)
  (let* ((dial-rotated (modulo (+ dial rotation) 100))
         (delta (if (zero? dial-rotated) 1 0)))
    (values dial-rotated delta)))

(display (entrance-password rotate-simple 50 0))
(newline)
