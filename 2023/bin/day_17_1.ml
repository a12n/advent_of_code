open Advent
open Day_17

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  let n_rows, n_cols = Grid.size grid in
  let _, heat_loss = Option.get (Grid.min_path grid (0, 0) (n_rows - 1, n_cols - 1)) in
  print_endline (string_of_int heat_loss)
