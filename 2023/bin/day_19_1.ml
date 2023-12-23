open Day_19
open Advent

let () =
  let lines = input_lines stdin in
  let sys = System.of_lines lines in
  Seq.map Part.of_string lines
  |> Seq.filter (System.eval sys)
  |> Seq.map Part.rating |> Seq.reduce ( + ) |> string_of_int |> print_endline
