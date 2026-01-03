(define-library (advent opt)
  (export simplex)

  (import (scheme base)
          (srfi 43)
          (advent matrix))

  (begin

    ;; Maximize objective `P = c x` subject to `A x ≤ b^T` and
    ;; `x ≥ 0`.
    ;;
    ;; Matrix `A` is of size N×M (N — number of constraints, M — number of
    ;; variables). Values `b` and `c` are expected to be 1×N and 1×M vectors.
    ;;
    ;; Return values:
    ;; 1. Optimal value of the objective function
    ;; 2. The corresponding `x^T` vector
    ;;
    ;; Returns #f as objective value for infeasible problem.
    ;; Returns +inf.0 objective value for unbounded problem.
    ;;
    ;; If there's an equality in the model:
    ;;  3 x + 4 y = 9  <=>
    ;;  3 x + 4 y ≤ 9
    ;;  3 x + 4 y ≥ 9
    ;;
    ;; If there's "greater or equal" inequality in the model:
    ;;  3 x + 4 y ≥  9  <=>
    ;; -3 x - 4 y ≤ -9
    ;;
    ;; For example, minimize
    ;;  P = 6 x₁ + 3 x₂
    ;; subject to lower bounds
    ;;  x₁ ≥ 0
    ;;  x₂ ≥ 0
    ;; subject to
    ;;  x₁ + x₂ ≥ 1
    ;;  2 x₁ - x₂ ≥ 1
    ;;  3 x₂ ≤ 2
    ;;
    ;; Reformulate objective, maximize
    ;;  Q = -P = -6 x₁ - 3 x₂
    ;;
    ;; Reformulate "greater or equal" constraints:
    ;;  -x₁ - x₂ ≤ -1
    ;;  -2 x₁ + x₂ ≤ -1
    ;;  3 x₂ ≤ 2
    ;;
    ;; (let ((a #(#(-1 -1)
    ;;            #(-2  1)
    ;;            #( 0  3)))
    ;;       (b #(-1 -1 2))
    ;;       (c #(-6 -3)))
    ;;   (let-values (((q x) (simplex a b c)))
    ;;     (display "Optimal P ")
    ;;     (display (- q))
    ;;     (newline)))
    (define (simplex a b c)
      (let-values (((basis tableau) (simplex-tableau a b c)))
        (let ((x (make-vector (vector-length c) 0)))
          (cond
           ((not tableau) (values #false x))
           ((equal? tableau +inf.0) (values +inf.0 x))
           (else
            (let ((bounded (simplex-pivoting! basis tableau)))
              (cond
               ((not bounded) (values +inf.0 x))
               (else
                (vector-for-each
                 (lambda (i bi)
                   (when (< bi (vector-length x))
                     (vector-set! x bi (matrix-ref tableau i 0))))
                 basis)
                (values (matrix-ref tableau (- (matrix-rows tableau) 1) 0) x)))))))))

    (define (simplex-tableau a b c)
      (let* ((n-constr (matrix-rows a))
             (n-var (matrix-cols a))
             ;; Since all constraints are "less than or equal", there
             ;; will be a slack variable for each constraint.
             (n-slack n-constr)
             ;; Each negative entry in b will require an artificial variable.
             ;; If n-artif isn't zero, phase 1 will be needed to find an initial BFS.
             (n-artif (vector-fold
                       (lambda (_ n bi)
                         (if (negative? bi)
                             (+ n 1) n))
                       0 b)))
        (if (not (= (vector-length b) n-constr))
            (error "invalid size" b))
        (if (not (= (vector-length c) n-var))
            (error "invalid size" c))
        (let ((basis (make-vector n-constr 0))
              (tableau (make-matrix
                        (+ n-constr (if (positive? n-artif) 1 0) 1)
                        (+ 1 n-var n-slack n-artif)
                        0)))
          ;; Pre-augment the tableau matrix with b. Usually it's the
          ;; last column (i.e., the RHS), but here it's on the LHS to have
          ;; artificial variables in last columns.
          ;;      b
          ;; #(#(-1)
          ;;   #(-1)
          ;;   #( 2)
          (matrix-map-col!
           (lambda (i _)
             (vector-ref b i))
           tableau 0
           0 n-constr)
          ;; Copy constraints into the tableau.
          ;;      b x₁ x₂
          ;; #(#(-1 -1 -1)
          ;;   #(-1 -2  1)
          ;;   #( 2  0  3)
          (matrix-map!
           (lambda (i j _)
             (matrix-ref a i (- j 1)))
           tableau
           0 n-constr
           1 (+ 1 n-var))
          ;; Add objective row (negated, since variables moved to the variable side):
          ;;  Q = -6 x₁ - 3 x₂
          ;;  Q + 6 x₁ + 3 x₂ = 0
          ;;      b x₁ x₂
          ;; #(#(-1 -1 -1)
          ;;   #(-1 -2  1)
          ;;   #( 2  0  3)
          ;;   #( 0  6  3))
          (matrix-map-row!
           (lambda (j _)
             (- (vector-ref c (- j 1))))
           tableau n-constr
           1 (+ 1 n-var))
          ;; Add slack variables. Initially these variables are in the
          ;; basis:
          ;;      b x₁ x₂ s₁ s₂ s₃
          ;; #(#(-1 -1 -1  1  0  0)
          ;;   #(-1 -2  1  0  1  0)
          ;;   #( 2  0  3  0  0  1)
          ;;   #( 0  6  3  0  0  0))
          (do ((k 0 (+ k 1)))
              ((= k n-slack) #f)
            ;; Numbers in the basis are zero-based (it's the number of
            ;; variable, not the number of tableau column).
            (vector-set! basis k (+ n-var k))
            (matrix-set! tableau k (+ 1 n-var k) 1))
          ;; For each negative in b, add artificial variable. There
          ;; will be phase 1, and artificial variables will be in the initial
          ;; basis:
          ;;      b x₁ x₂ s₁ s₂ s₃ a₁ a₂
          ;; #(#(-1 -1 -1  1  0  0 -1  0)
          ;;   #(-1 -2  1  0  1  0  0 -1)
          ;;   #( 2  0  3  0  0  1  0  0)
          ;;   #( 0  6  3  0  0  0  0  0))
          ;; (do ((k 0 (+ k 1)))
          ;;     ((= k n-artif) #f)
          ;;   ;; FIXME: Don't change all n-artif rows, change only specific rows.
          ;;   (when (negative? (matrix-ref tableau k 0))
          ;;     (vector-set! basis k (+ n-var n-slack k))
          ;;     (matrix-set! tableau k (+ 1 n-var n-slack k) -1)))
          (let loop1 ((i 0)
                      (k 0))
            (cond
             ((= i n-constr) #f)
             ((negative? (matrix-ref tableau i 0))
              (vector-set! basis i (+ n-var n-slack k))
              (matrix-set! tableau i (+ 1 n-var n-slack k) -1)
              (loop1 (+ i 1) (+ k 1)))
             (else (loop1 (+ i 1) k))))
          ;; Negate constraints with artificial variables (i.e.,
          ;; constraints where b is negative):
          ;;      b x₁ x₂ s₁ s₂ s₃ a₁ a₂
          ;; #(#( 1  1  1 -1  0  0  1  0)
          ;;   #( 1  2 -1  0 -1  0  0  1)
          ;;   #( 2  0  3  0  0  1  0  0)
          ;;   #( 0  6  3  0  0  0  0  0))
          ;; (matrix-map!
          ;;  (lambda (i _ x)
          ;;    ;; FIXME: Don't change all n-artif rows.
          ;;    (if (negative? (matrix-ref tableau i 0))
          ;;        (- x) x))
          ;;  tableau
          ;;  0 n-artif
          ;;  0 (matrix-cols tableau))
          (let loop2 ((i 0)
                      (k 0))
            (cond
             ((= i n-constr) #f)
             ((negative? (matrix-ref tableau i 0))
              (matrix-set! tableau (+ n-constr 1) (+ 1 n-var n-slack k) 1)
              (matrix-elim-add! tableau (+ n-constr 1) i 1)
              (matrix-map-row! (lambda (_ x) (- x)) tableau i)
              (loop2 (+ i 1) (+ k 1)))
             (else (loop2 (+ i 1) k))))
          ;; Add artificial objective row.
          ;; Minimize R = a₁ + a₂,
          ;; maximize S = -R = -a₁ - a₂,
          ;; or S + a₁ + a₂ = 0.
          ;;      b x₁ x₂ s₁ s₂ s₃ a₁ a₂
          ;; #(#( 1  1  1 -1  0  0  1  0)
          ;;   #( 1  2 -1  0 -1  0  0  1)
          ;;   #( 2  0  3  0  0  1  0  0)
          ;;   #( 0  6  3  0  0  0  0  0)
          ;;   #( 0  0  0  0  0  0  1  1))
          ;; (do ((k 0 (+ k 1)))
          ;;     ((= k n-artif) #f)
          ;;   (matrix-set! tableau (+ n-constr 1) (+ 1 n-var n-slack k) 1))
          ;; Express artificial objective row in terms of non-basic
          ;; variables. Subtract from the artificial objective row the rows of
          ;; artificial basis:
          ;;      b x₁ x₂ s₁ s₂ s₃ a₁ a₂
          ;; #(#( 1  1  1 -1  0  0  1  0)
          ;;   #( 1  2 -1  0 -1  0  0  1)
          ;;   #( 2  0  3  0  0  1  0  0)
          ;;   #( 0  6  3  0  0  0  0  0)
          ;;   #(-2 -3  0  1  1  0  0  0))
          ;; (do ((i 0 (+ i 1)))
          ;;     ((= i n-artif) #f)
          ;;   (do ((j 0 (+ j 1)))
          ;;       ((= j (matrix-cols tableau)) #f)
          ;;     (matrix-set! tableau (+ n-constr 1) j
          ;;                  (- (matrix-ref tableau (+ n-constr 1) j)
          ;;                     (matrix-ref tableau i j)))))
          (if (> n-artif 0)
              ;; Optimize artificial objective first.
              (let ((bounded (simplex-pivoting! basis tableau)))
                (cond
                 ((not bounded)
                  ;; Unbounded problem.
                  (values basis +inf.0))
                 ((not (zero? (matrix-ref tableau (- (matrix-rows tableau) 1) 0)))
                  ;; Couldn't minimize artificial variables. Infeasible problem.
                  (values basis #false))
                 (else
                  (simplex-tableau-cleanup! basis tableau n-var)
                  ;; Found a BFS. Drop artificial variables and objective.
                  ;; The tableau is configured for the original problem starting at this BFS.
                  (values basis
                          (matrix-copy tableau
                                       0 (+ n-constr 1)
                                       0 (+ 1 n-var n-slack))))))
              ;; Already at BFS at zero. Return the tableau for original problem.
              (values basis tableau)))))

    ;; Get artificial variables out of the basis after phase 1.
    ;; https://math.stackexchange.com/questions/3254444/artificial-variables-in-two-phase-simplex-method
    (define (simplex-tableau-cleanup! basis tableau n-var)
      (let* ((n-constr (vector-length basis))
             (n-slack n-constr)
             (n-artif (- (matrix-cols tableau) 1 n-var n-slack)))
        ;; After phase 1 there might be artificial variables in the
        ;; basis. E.g., a₁ in the following tableau:
        ;;       b x₁ x₂   s₁   s₂ s₃ s₄ a₁ a₂
        ;; #(#(  1  0  1  1/2 -1/2  0  0  0  0)
        ;;   #( 12  0  0 -3/2    4  0  1  0 -1)
        ;;   #(  0  0  0    0   -1 -1  0  1  0)
        ;;   #(  4  1  0 -1/2    1  0  0  0  0)
        ;;   #(-23  0  0    1 -7/2  0  0  0  0)
        ;;   #(  0  0  0    0    1  1  0  0  1))
        (vector-map!
         (lambda (pi bi)
           (let ((pj (and
                      ;; Artificial variable in the basis.
                      (>= bi (+ n-var n-slack))
                      ;; Pivot on any non-zero non-artificial
                      ;; variable in this row. If there's no such
                      ;; variable, the row may be removed.
                      (matrix-fold
                       (lambda (_ j k x)
                         (if (or k (zero? x)) k j))
                       #false
                       tableau
                       pi (+ pi 1)
                       1 (+ 1 n-var n-slack 1)))))
             (cond
              (pj (matrix-pivot! tableau pi pj) pj)
              (else bi))))
         basis)))

    ;; Index of the pivot column in objective row `i` of the tableau.
    ;; Returns #false if the objective couldn't be improved further.
    ;; TODO: Bland's rule?
    (define (pivot-col tableau i)
      (matrix-fold
       (lambda (_ j k x)
         (if (and (negative? x)
                  (or (not k)
                      (< x (matrix-ref tableau i k))))
             j k))
       #false
       tableau
       i (+ i 1)
       1 (matrix-cols tableau)))

    ;; Index of the pivot row in one of `n` constraint rows at column
    ;; `j`. Returns #false if there's no such row (the problem is unbounded).
    (define (pivot-row tableau n-constr j)
      (car
       (matrix-fold
        (lambda (i _ state x)
          (let* ((k (car state))
                 (thetak (cdr state))
                 (bi (matrix-ref tableau i 0))
                 (thetai (if (zero? x) +inf.0 (/ bi x))))
            (if (and (not (negative? thetai))
                     (finite? thetai)
                     (or (not k)
                         (< thetai thetak)))
                (cons i thetai) state)))
        '(#false . +inf.0)
        tableau
        0 n-constr
        j (+ j 1))))

    (define (simplex-pivoting! basis tableau)
      (let* ((pj (pivot-col tableau (- (matrix-rows tableau) 1)))
             (pi (and pj (pivot-row tableau (vector-length basis) pj))))
        (cond
         ((not pj)
          ;; Can't find pivot column. The configuration in tableau is optimal.
          #true)
         ((not pi)
          ;; Can't select pivot row. The problem is unbounded.
          #false)
         (else
          ;; Do pivoting.
          (vector-set! basis pi (- pj 1))
          (matrix-pivot! tableau pi pj)
          (simplex-pivoting! basis tableau)))))

    ))
