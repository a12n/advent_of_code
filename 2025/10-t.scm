#!/usr/bin/env gosh

(import (scheme base)
        (srfi 78)
        (advent opt))

;; Convert values to cons cell
(define (simplex* a b c)
  (let-values (((z x) (simplex a b c)))
    (cons z x)))

(check-set-mode! 'report-failed)

(check (simplex*
        #(#(2 1 0)
          #(0 2 1)
          #(1 0 2))
        #(10 20 30)
        #(3 4 5))
       => '(90 . #(10/3 10/3 40/3)))

(check (simplex*
        #(#(-1 -1)
          #(-2  1)
          #( 0  3))
        #(-1 -1 2)
        #(-6 -3))
       => '(-5 . #(2/3 1/3)))

;; https://en.wikipedia.org/wiki/Simplex_algorithm#Example
(check (simplex*
        #(#(3 2 1)
          #(2 5 3))
        #(10 15)
        #(2 3 4))
       => '(20 . #(0 0 5)))

;; https://en.wikipedia.org/wiki/Simplex_algorithm#Example_2
(check (simplex*
        #(#( 3  2  1)
          #(-3 -2 -1)
          #( 2  5  3)
          #(-2 -5 -3))
        #(10 -10 15 -15)
        #(2 3 4))
       => '(130/7 . #(15/7 0 25/7)))

(check (simplex*
        #(#(1 1)
          #(2 1))
        #(12 16)
        #(40 30))
       => '(400 . #(4 8)))

(check (simplex*
        #(#(2 3 0)
          #(0 2 5)
          #(3 2 4))
        #(8 10 15)
        #(3 5 4))
       => '(765/41 . #(89/41 50/41 62/41)))

(check (simplex*
        #(#(3 2)
          #(3 5)
          #(5 6))
        #(600 800 1100)
        #(30 40))
       => '(7000 . #(100 100)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=sm&q1=bme1
(check (simplex*
        #(#(-2 -4)
          #(-1 -7))
        #(-4 -7)
        #(-1 -1))
       => '(-1 . #(0 1)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=sm&q1=bme2
(check (simplex*
        #(#( 2  4)
          #( 2  2)
          #(-2 -2)
          #(-5 -2))
        #(12 10 -10 -10)
        #(-5 -3))
       => '(-23 . #(4 1)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=sm&q1=ms1
(check (simplex*
        #(#( 2  3)
          #( 3  2)
          #(-1 -1))
        #(30 24 -3)
        #(6 4))
       => '(48 . #(8 0)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=sm&q1=is1
(check (simplex*
        #(#(1  1)
          #(0 -1))
        #(5 -8)
        #(6 4))
       => '(#false . #(0 0)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=sm&q1=us1
(check (simplex*
        #(#(1 -2)
          #(1  0)
          #(0 -1))
        #(6 10 -1)
        #(3 5))
       => '(+inf.0 . #(0 0)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=tp&q1=E1
(check (simplex*
        #(#(-2 -1)
          #(-1 -7))
        #(-4 -7)
        #(-1 -1))
       => '(-31/13 . #(21/13 10/13)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=tp&q1=E2
(check (simplex*
        #(#(1  0 -1)
          #(0 -1 -1))
        #(10 -10)
        #(-5 -2 -10))
       => '(-20 . #(0 10 0)))

;; https://cbom.atozmath.com/example/CBOM/Simplex.aspx?q=tp&q1=IS1
(check (simplex*
        #(#(-2  1  3)
          #( 2 -1 -3)
          #( 2  3  4)
          #(-2 -3 -4))
        #(2 -2 1 -1)
        #(-1 2 3))
       => '(#false . #(0 0 0)))

;; https://www.ams.jhu.edu/~castello/625.414/Handouts/TwoPhase.pdf
(check (simplex*
        #(#( 2  1  2)
          #(-2 -1 -2)
          #( 3  3  1)
          #(-3 -3 -1))
        #(4 -4 3 -3)
        #(-4 -1 -1))
       => '(-22/10 . #(0 4/10 18/10)))
