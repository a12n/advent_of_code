open Advent
open Day_17

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  let _, heat_loss = Grid.min_path grid in
  print_endline (string_of_int heat_loss)
