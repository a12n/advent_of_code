open Advent
open Day_12

let () =
  input_lines stdin
  |> Seq.map Pattern.(arrangements % unfold % of_string)
  |> Seq.reduce ( + ) |> string_of_int |> print_endline
