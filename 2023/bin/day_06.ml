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
