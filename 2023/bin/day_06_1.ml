open Advent
open Day_06

let () =
  let times, dists = parse_input int_list_of_string stdin in
  let product =
    List.map
      (fun (race_time, record_dist) ->
        Seq.ints 0
        |> Seq.take (race_time + 1)
        |> Seq.map (distance race_time)
        |> Seq.filter (fun dist -> dist > record_dist)
        |> Seq.length)
      (List.combine times dists)
    |> List.reduce ( * )
  in
  print_endline (string_of_int product)
