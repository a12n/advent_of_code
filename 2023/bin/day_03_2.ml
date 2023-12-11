open Advent
open Day_03

let () =
  let State.{ parts; symbols; _ } =
    input_chars stdin
    |> Seq.fold_left State.update State.initial
    |> State.finish
  in
  let sum =
    List.filter (fun (gear, _) -> gear) symbols
    |> List.filter_map (fun sym ->
           match List.filter ((Fun.flip Part.is_adjacent) sym) parts with
           | Part.[ { num = a; _ }; { num = b; _ } ] -> Some (a * b)
           | _ -> None)
    |> List.reduce ( + )
  in
  print_endline (string_of_int sum)
