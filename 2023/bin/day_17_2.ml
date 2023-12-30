open Advent
open Day_17
open Grid.Ops

let () =
  let grid = Heat_Grid.of_lines (input_lines stdin) in
  let n_rows, n_cols = Heat_Grid.size grid in
  let src, dest = ((0, 0), (n_rows - 1, n_cols - 1)) in
  let costs, dirs = Heat_Grid.distance grid src in
  let path = Heat_Grid.backtrack dirs dest in
  Heat_Grid.pp_dist ~path Format.err_formatter grid costs dirs;
  Heat_Grid.pp ~path Format.err_formatter grid;
  print_endline (string_of_int costs.@(dest))
