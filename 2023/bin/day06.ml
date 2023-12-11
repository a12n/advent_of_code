open Advent

let distance race_time hold_time =
  assert (race_time > 0);
  assert (hold_time >= 0 && hold_time <= race_time);
  (race_time - hold_time) * hold_time

let part1 () =
  let records =
    let int_list s =
      String.split_on_char ' ' s
      |> List.filter (( <> ) "")
      |> List.map int_of_string
    in
    let time = Scanf.scanf "Time: %[^\n]" int_list in
    let distance = Scanf.scanf "Distance: %[^\n]" int_list in
    List.combine time distance
  in
  let product =
    List.map
      (fun (race_time, record_dist) ->
        Seq.ints 0
        |> Seq.take (race_time + 1)
        |> Seq.map (distance race_time)
        |> Seq.filter (fun dist -> dist > record_dist)
        |> Seq.length)
      records
    |> List.reduce ( * )
  in
  print_endline (string_of_int product)

let part2 () = ()

let () = (parse_args Sys.argv [| part1; part2 |]) ()
