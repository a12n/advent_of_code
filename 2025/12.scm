#!/usr/bin/env gosh

(import (scheme base)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (srfi 43)
        (scheme write)
        (advent input))

(define (split pred list)
  (let-values (((a b) (break pred list)))
    (cons a (if (null? b) '()
                (split pred (cdr b))))))

(define (lines->shape list)
  (unless (= (length list) 4)
    (error "invalid shape lines" list))
  (cons (string->shape-index (car list))
        (cdr list)))

(define (string->shape-index s)
  (let ((n (string-length s)))
    (unless (and (> n 1)
                 (eq? (string-ref s (- n 1)) #\:))
      (error "invalid shape index" s))
    (string->number (string-take s (- n 1)))))

(define (string->region s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\:)))))
    (unless (= (length fields) 2)
      (error "invalid region" s))
    (cons (string->region-size (car fields))
          (string->shape-quantity (cadr fields)))))

(define (string->region-size s)
  (let ((fields (string-tokenize s (char-set-complement (char-set #\x)))))
    (unless (= (length fields) 2)
      (error "invalid region size" s))
    (cons (string->number (car fields))
          (string->number (cadr fields)))))

(define (string->shape-quantity s)
  (list->vector
   (map string->number (string-tokenize s (char-set-complement char-set:whitespace)))))

(define (lines->input list)
  (let ((groups (split string-null? list)))
    (cons (map lines->shape (drop-right groups 1))
          (map string->region (last groups)))))

(define (read-input)
  (lines->input (read-lines)))

(define (shape-area shape)
  (fold (lambda (s area)
          (+ area (string-count s #\#)))
        0 shape))

(let* ((input (read-input))
       (shapes (map cdr (car input)))
       (shape-areas (list->vector (map shape-area shapes)))
       (regions (cdr input)))
  (write (list "shapes" shapes) (current-error-port))
  (newline (current-error-port))

  (write (list "areas" shape-areas) (current-error-port))
  (newline (current-error-port))

  (write (list "regions" regions) (current-error-port))
  (newline (current-error-port))

  (display
   (fold
    (lambda (region sum)
      (let* ((size (car region))
             (quantity (cdr region))
             (area-total (* (car size) (cdr size)))
             (area-req (vector-fold
                        (lambda (i area n)
                          (+ area (* n (vector-ref shape-areas i))))
                        0 quantity)))
        (display (list "size" size
                       "quantity" quantity
                       "area-total" area-total
                       "area-req" area-req
                       "cant-fit" (> area-req area-total))
                 (current-error-port))
        (newline (current-error-port))
        (+ sum (if (> area-req area-total) 0 1))
        ))
    0 regions))
  (newline)

  )
