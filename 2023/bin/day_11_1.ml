open Advent
open Day_11

let () =
  let img = Image.of_chars (input_chars stdin) in
  (* TODO *)
  List.iter (fun (row, col) -> Printf.printf "%d %d\n" row col) img
