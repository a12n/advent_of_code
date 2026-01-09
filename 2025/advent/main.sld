(define-library (advent main)
  (export main)
  (import (scheme base)
          (scheme cxr)
          (scheme process-context)
          (srfi 13))
  (begin
    (define (main . parts)
      (let ((name (car (command-line))))
        (cond
         ((string-suffix? "-1" name) ((car parts)))
         ((string-suffix? "-2" name) ((cadr parts)))
         ((string-suffix? "-t" name) ((caddr parts))))))))
