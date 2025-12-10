#!/usr/bin/env gosh

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (srfi 151)
        (advent input)
        (advent main))

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
(define (button->button-bits button)
  (fold (lambda (k bits)
          (bitwise-ior bits (arithmetic-shift 1 k)))
        0 button))

;; Buttons in the provided buttons list must be in bitmask
;; representation.
(define (num-button-presses goal-lights buttons)
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
             (buttons (map button->button-bits buttons)))
        (+ sum (num-button-presses lights buttons))))
    0))
  (newline))

;; ---------------------------------------------------------------------------
;; Part 2

(define (part-2)
  ;; TODO
  )

;; ---------------------------------------------------------------------------
;; Main

(main part-1 part-2)
