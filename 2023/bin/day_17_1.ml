open Advent
open Day_17

let () =
  let grid = Heat_Grid.of_lines (input_lines stdin) in
  let n_rows, n_cols = Heat_Grid.size grid in
  let loss = Heat_Grid.heat_loss ~max_straight:3 grid (0, 0) (n_rows - 1, n_cols - 1) in
  print_endline (string_of_int loss)
