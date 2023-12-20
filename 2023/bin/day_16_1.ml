open Advent
open Day_16

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  Grid.trace grid Dir.Right (0, 0) |> Energized_Map.length |> string_of_int |> print_endline
