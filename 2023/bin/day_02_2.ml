open Advent
open Day_02

let () =
  let sum =
    input_lines stdin |> Seq.map Game.of_string
    |> Seq.map (fun Game.{ draws; _ } -> List.reduce Draw.max_elts draws)
    |> Seq.map Draw.power |> Seq.reduce ( + )
  in
  print_endline (string_of_int sum)
