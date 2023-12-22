open Advent
open Day_10

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  Grid.pp Format.err_formatter grid;
  Grid.cycle grid |> Option.get |> Polygon.area |> string_of_int |> print_endline
