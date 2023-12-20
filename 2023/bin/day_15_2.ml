open Advent
open Day_15

let () =
  let boxes = Boxes.make () in
  read_line () |> String.split_on_char ',' |> List.map Step.of_string
  |> List.iter (Boxes.perform boxes);
  print_endline (string_of_int (Boxes.focus_power boxes))
