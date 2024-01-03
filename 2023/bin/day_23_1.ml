open Advent
open Day_23

let () =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  let start, finish = Trail_Map.(start trails, finish trails) in
  Printf.eprintf "start (%d, %d), finish (%d, %d)\n%!" (fst start) (snd start) (fst finish)
    (snd finish);
  (* TODO *)
  Trail_Map.pp Format.err_formatter trails
