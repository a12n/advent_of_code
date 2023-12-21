open Advent
open Day_18

let () =
  let plan = Plan.of_lines (input_lines stdin) in
  let grid = Grid.of_plan plan in
  Grid.dig_interior grid;
  print_endline (string_of_int (Grid.trench_volume grid))
