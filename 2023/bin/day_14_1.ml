open Advent
open Day_14

let () =
  input_lines stdin |> Platform.of_lines
  |> (Fun.flip Platform.tilt) Dir.North
  |> Platform.load |> string_of_int |> print_endline
