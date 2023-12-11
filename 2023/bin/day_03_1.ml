open Advent
open Day_03

let () =
  let State.{ parts; symbols; _ } =
    input_chars stdin
    |> Seq.fold_left State.update State.initial
    |> State.finish
  in
  List.iter prerr_endline (List.map Part.to_string parts);
  List.iter prerr_endline (List.map Symbol.to_string symbols);
  let sum =
    List.filter
      (fun part -> List.exists (Part.is_adjacent part) symbols)
      parts (* All parts adjacent to at least one symbol. *)
    |> List.map (fun Part.{ num; _ } -> num) (* Extract the part numbers. *)
    |> List.reduce ( + ) (* Sum the part numbers. *)
  in
  print_endline (string_of_int sum)
