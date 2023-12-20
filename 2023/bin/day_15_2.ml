open Advent
open Day_15

let () =
  let steps = read_line () |> String.split_on_char ',' |> List.map Step.of_string in
  List.iter
    (fun Step.{ label; op } ->
      Printf.eprintf "\"%s\" : " label;
      match op with
      | `Remove -> Printf.eprintf ", remove\n"
      | `Insert fd -> Printf.eprintf ", insert %d\n" fd)
    steps
