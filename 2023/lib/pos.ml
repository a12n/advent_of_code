type t = int * int

let add (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)
let add_dir pos dir = add pos (Dir.to_pos dir)
let sub (row1, col1) (row2, col2) = (row1 - row2, col1 - col2)
