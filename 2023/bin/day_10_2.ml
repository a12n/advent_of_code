open Advent
open Day_10

let () =
  Grid.(cycle (of_lines (input_lines stdin)))
  |> Option.get |> Polygon.area |> string_of_int |> print_endline
