open Advent

let () =
  Day_13.input stdin
  |> Seq.filter_map Day_13.Grid.reflection
  |> Seq.map (function `Horiz i -> 100 * i | `Vert i -> i)
  |> Seq.reduce ( + ) |> string_of_int |> print_endline
