open Advent
open Day_04

let () =
  let sum =
    input_lines stdin |> Seq.map Card.of_string
    |> Seq.map (fun (_, matching) ->
           match Numbers.cardinal matching with 0 -> 0 | n -> 1 lsl (n - 1))
    |> Seq.reduce ( + )
  in
  print_endline (string_of_int sum)
