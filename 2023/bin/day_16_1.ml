open Advent
open Day_16

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  let energized = Grid.trace grid (0, 0) Dir.Right in
  print_endline (string_of_int (Energized_Map.length energized))
