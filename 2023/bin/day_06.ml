open Advent

let distance race_time hold_time =
  assert (race_time > 0);
  assert (hold_time >= 0 && hold_time <= race_time);
  (race_time - hold_time) * hold_time

let int_list_of_string s =
  String.split_on_char ' ' s
  |> List.filter (( <> ) "")
  |> List.map int_of_string

let int_of_string' s =
  String.trim s |> String.map (function ' ' -> '_' | c -> c) |> int_of_string

let parse_input f ch =
  let pair s =
    match String.split_on_char ':' s with
    | [ key; value ] -> String.(trim key, trim value)
    | _ -> invalid_arg __FUNCTION__
  in
  let time =
    match pair (input_line ch) with
    | "Time", numbers -> f numbers
    | _ -> invalid_arg __FUNCTION__
  in
  let distance =
    match pair (input_line ch) with
    | "Distance", numbers -> f numbers
    | _ -> invalid_arg __FUNCTION__
  in
  (time, distance)

let part1 () =
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

let part2 () =
  let time, record_dist = parse_input int_of_string' stdin in
  let num =
    Seq.ints 0
    |> Seq.take (time + 1)
    |> Seq.map (distance time)
    |> Seq.filter (fun dist -> dist > record_dist)
    |> Seq.length
  in
  print_endline (string_of_int num)

let () = (parse_args Sys.argv [| part1; part2 |]) ()
