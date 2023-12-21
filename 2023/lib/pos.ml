type t = int * int

let add (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)
let add_dir pos dir = add pos (Dir.to_pos dir)
let sub (row1, col1) (row2, col2) = (row1 - row2, col1 - col2)

(** Two points are on a horizontal line. *)
let is_horiz_aligned (row1, _) (row2, _) = row1 = row2

(** Two points are on a vertical line. *)
let is_vert_aligned (_, col1) (_, col2) = col1 = col2

(** Two points are on a horizontal or on a vertical line. *)
let is_aligned p0 p1 = is_horiz_aligned p0 p1 || is_vert_aligned p0 p1
