open Advent
open Day_17

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  let n_rows, n_cols = Grid.size grid in
  (* let loss, path = Option.get (Grid.path grid (0, 0) (n_rows - 1, n_cols - 1)) in *)
  let loss = Grid.heat_loss grid (0, 0) (n_rows - 1, n_cols - 1) in
  let path = [] in
  Grid.pp ~path Format.err_formatter grid;
  print_endline (string_of_int loss)
