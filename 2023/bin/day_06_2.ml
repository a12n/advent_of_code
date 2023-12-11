open Advent
open Day_06

let () =
  let time, record_dist = parse_input int_of_string' stdin in
  let num =
    Seq.ints 0
    |> Seq.take (time + 1)
    |> Seq.map (distance time)
    |> Seq.filter (fun dist -> dist > record_dist)
    |> Seq.length
  in
  print_endline (string_of_int num)
