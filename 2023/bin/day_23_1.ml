open Advent
open Day_23

let () =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  (* TODO *)
  Trail_Map.pp Format.err_formatter trails
