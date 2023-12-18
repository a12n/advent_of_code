open Advent
open Day_13

let () =
  input stdin |> Seq.filter_map Grid.reflection
  |> Seq.map (function `Horiz i -> i | `Vert i -> 100 * i)
  |> Seq.reduce ( + ) |> string_of_int |> print_endline
