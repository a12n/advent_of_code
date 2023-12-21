open Advent
open Day_18

let () =
  let plan = Plan.of_lines2 (input_lines stdin) in
  let (min_row, min_col), (max_row, max_col) = Plan.min_max_pos plan in
  let n_rows, n_cols = (max_row - min_row + 1, max_col - min_col + 1) in
  Printf.eprintf "%dx%d: (%d, %d) -> (%d, %d)" n_rows n_cols min_row min_col max_row max_col
