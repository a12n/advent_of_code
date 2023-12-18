open Advent

let () =
  Day_13.input stdin
  |> Seq.filter_map Day_13.Grid.reflection
  |> Seq.map (function `Horiz k -> 100 * k | `Vert k -> k)
  |> Seq.reduce ( + ) |> string_of_int |> print_endline
