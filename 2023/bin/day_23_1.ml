open Advent
open Day_23

let () =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  let start, finish = Trail_Map.(start trails, finish trails) in
  Printf.eprintf "start (%d, %d), finish (%d, %d)\n%!" (fst start) (snd start) (fst finish)
    (snd finish);
  Trail_Map.pp Format.err_formatter trails;
  let hikes = Trail_Map.hikes trails start finish in
  let longest =
    List.reduce
      (fun hike1 hike2 -> if Pos_Set.(cardinal hike2 > cardinal hike1) then hike2 else hike1)
      hikes
  in
  print_endline (string_of_int (Pos_Set.cardinal longest))
