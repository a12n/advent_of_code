open Advent
open Day_14

let () =
  input_lines stdin |> Platform.of_lines |> Platform.tilt |> Platform.load |> string_of_int
  |> print_endline
