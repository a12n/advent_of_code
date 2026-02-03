#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (srfi 69)
        (srfi 113)
        (srfi 114)
        (srfi 133)
        (srfi 134)
        (srfi 151)
        (only (advent geometry)
              vector-add vector-sub
              vector-norm-max)
        (advent input)
        (advent main)
        (advent matrix)
        (advent opt))

(define (string->lights-diagram s)
  (let ((n (string-length s)))
    (if (or (< n 3)
            (not (char=? (string-ref s 0) #\[))
            (not (char=? (string-ref s (- n 1)) #\])))
        (error "invalid string" s))
    (string-fold-right
     (lambda (char lights)
       (bitwise-ior (arithmetic-shift lights 1)
                    (case char
                      ((#\#) 1)
                      ((#\.) 0)
                      (else (error "invalid string" s)))))
     0 s 1 (- n 1))))

(define (string->number-list s)
  (let ((n (string-length s)))
    (if (or (< n 2)
            (let ((first (string-ref s 0))
                  (last (string-ref s (- n 1))))
              (not (or (and (char=? first #\() (char=? last #\)))
                       (and (char=? first #\{) (char=? last #\}))))))
        (error "invalid string" s))
    (map string->number (string-tokenize s (char-set-complement (char-set #\,)) 1 (- n 1)))))

(define (string->machine-descr s)
  (let ((fields (string-tokenize s)))
    (if (< (length fields) 3)
        (error "invalid string" s))
    (let* ((lights (string->lights-diagram (car fields)))
           (number-lists (reverse (map string->number-list (cdr fields))))
           (joltage-reqs (list->vector (car number-lists)))
           (buttons (reverse (cdr number-lists))))
      (vector lights buttons joltage-reqs))))

;; ---------------------------------------------------------------------------
;; Part 1

;; Button is a list of indices (indices of the toggled lights in part
;; 1, and indices of joltage levels in part 2). Button bits is bitmask
;; representation of the list.
(define (button->light-bits button)
  (fold (lambda (k bits)
          (bitwise-ior bits (arithmetic-shift 1 k)))
        0 button))

;; Buttons in the provided buttons list must be in bitmask
;; representation.
(define (num-light-button-presses goal-lights buttons)
  (let loop ((solution #f)
             (states (ideque #(0 0)))
             (seen (set exact-integer-comparator)))
    (if (ideque-empty? states) solution
        (let* ((state (ideque-front states))
               (states (ideque-remove-front states))
               (lights (vector-ref state 0))
               (presses (vector-ref state 1)))
          (cond
           ;; There's a solution and current number of presses is
           ;; already worse, drop this state.
           ((and solution (> presses solution)) (loop solution states seen))
           ;; Have already seen this state, drop.
           ((set-contains? seen lights) (loop solution states seen))
           ;; Goal condition met, update the solution and inspect rest of the states.
           ((= lights goal-lights) (loop presses states (set-adjoin seen lights)))
           ;; Transition to the neigbouring states.
           (else (loop solution
                       (fold (lambda (button states)
                               (ideque-add-back states
                                                (vector (bitwise-xor lights button)
                                                        (+ presses 1))))
                             states buttons)
                       (set-adjoin seen lights))))))))

(define (part-1)
  (display
   (fold-lines
    (lambda (line sum)
      (let* ((machine (string->machine-descr line))
             (lights (vector-ref machine 0))
             (buttons (vector-ref machine 1))
             (buttons (map button->light-bits buttons)))
        (+ sum (num-light-button-presses lights buttons))))
    0))
  (newline))

;; ---------------------------------------------------------------------------
;; Part 2

(define (joltage=? levels goal)
  (vector= = levels goal))

(define (joltage-overshoot? levels goal)
  (vector-any > levels goal))

;; buttons = #(0 0 0 1) #(0 1 0 1) #(0 0 1 0) #(0 0 1 1) #(1 0 1 0) #(1 1 0 0)
;; presses = #(n₀ n₁ n₂ n₃ n₄ n₅)
;; joltage-reqs = #(3 5 4 7))
;;
;;                5
;; joltage-reqs = Σ (n_i * buttons[i])
;;               i=0
;;
;; presses = ?
(define (num-joltage-button-presses joltage-reqs buttons)
  (define (loop cache levels)
    (display (list "cache" (hash-table-size cache)
                   "levels" levels)
             (current-error-port))
    (newline (current-error-port))
    (hash-table-ref
     cache levels
     (lambda ()
       (let ((min-presses
              (cond
               ((joltage-overshoot? levels joltage-reqs) +inf.0)
               ((joltage=? levels joltage-reqs) 0)
               (else
                (fold min +inf.0
                      (map (lambda (button)
                             (+ 1 (loop cache (vector-add levels button))))
                           buttons))))))
         (hash-table-set! cache levels min-presses)
         min-presses))))
  (exact (loop (make-hash-table)
               (make-vector (vector-length joltage-reqs) 0))))

(define (button->joltage n button)
  (let ((levels (make-vector n 0)))
    (for-each (lambda (i) (vector-set! levels i 1)) button)
    levels))

(define (part-2)
  (display
   (fold-lines
    (lambda (line sum)
      (let* ((machine (string->machine-descr line))
             (joltage-reqs (vector-ref machine 2))
             (buttons (vector-ref machine 1))
             (buttons (list->vector
                       (reverse (map (lambda (button)
                                       (button->joltage (vector-length joltage-reqs)
                                                        button))
                                     buttons)))))
        ;; Reformulate as LP problem:
        ;; a) Transpose buttons matrix, rewrite each "equal"
        ;; constraint as pair of "less than or equal" constraints.
        ;; b) Joltage requirements are part of the constraints, for
        ;; one original joltage requirement there are now negative and positive
        ;; requirements.
        ;; c) Cost vector is 1 (minimize) or -1 (maximize).
        (let ((a (matrix-unfold
                  (lambda (i j)
                    (if (even? i)
                        (matrix-ref buttons j (quotient i 2))
                        (- (matrix-ref buttons j (quotient (- i 1) 2)))))
                  (* 2 (matrix-cols buttons))
                  (matrix-rows buttons)))
              (b (vector-unfold
                  (lambda (i)
                    (if (even? i)
                        (vector-ref joltage-reqs (quotient i 2))
                        (- (vector-ref joltage-reqs (quotient (- i 1) 2)))))
                  (* 2 (vector-length joltage-reqs))))
              (c (make-vector (matrix-rows buttons) -1)))
          (let-values (((z x) (branch-bound a b c)))
            (display (list "z" z "x" x)
                     (current-error-port))
            (newline (current-error-port))
            (unless z
              (error "infeasible" a b c))
            (+ sum (- z))))))
    0))
  (newline))

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
