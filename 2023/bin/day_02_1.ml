open Advent
open Day_02

let () =
  let total = Draw.{ red = 12; green = 13; blue = 14 } in
  let sum =
    input_lines stdin |> Seq.map Game.of_string
    |> Seq.filter ((Fun.flip Game.is_possible) total)
    |> Seq.fold_left (fun sum Game.{ id; _ } -> sum + id) 0
  in
  print_endline (string_of_int sum)
