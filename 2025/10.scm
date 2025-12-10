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
    (string-fold
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

(define (number-list->schematic numbers)
  (fold (lambda (k schematic)
          (bitwise-ior schematic (arithmetic-shift 1 k)))
        0 numbers))

(define (string->machine-descr s)
  (let ((fields (string-tokenize s)))
    (if (< (length fields) 3)
        (error "invalid string" s))
    (let* ((lights (string->lights-diagram (car fields)))
           (number-lists (reverse (map string->number-list (cdr fields))))
           (joltage-reqs (car number-lists))
           (schematics (reverse (map number-list->schematic (cdr number-lists)))))
      (vector lights schematics joltage-reqs))))

;; ---------------------------------------------------------------------------
;; Part 1

(define (part-1)
  (display
   (fold-lines
    (lambda (line sum)
      (display (list "machine-descr" (string->machine-descr line)) (current-error-port))
      (newline (current-error-port))
      sum)
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
