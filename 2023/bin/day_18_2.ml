open Advent
open Day_18

let () =
  let plan = Plan.of_lines2 (input_lines stdin) in
  |> Plan.to_list_pos |> Polygon.area |> string_of_int |> print_endline
