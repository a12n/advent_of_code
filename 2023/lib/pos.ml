type t = int * int

let add (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)
let mul_int (row, col) n = (row * n, col * n)
let sub (row1, col1) (row2, col2) = (row1 - row2, col1 - col2)

(** Two points are on a horizontal line. *)
let is_horiz_aligned (row1, _) (row2, _) = row1 = row2

(** Two points are on a vertical line. *)
let is_vert_aligned (_, col1) (_, col2) = col1 = col2

(** Two points are on a horizontal or on a vertical line. *)
let is_aligned p0 p1 = is_horiz_aligned p0 p1 || is_vert_aligned p0 p1

let is_valid (n_rows, n_cols) (row, col) = row >= 0 && col >= 0 && row < n_rows && col < n_cols
let pp fmt (row, col) = Format.fprintf fmt "(%d, %d)" row col
