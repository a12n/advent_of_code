open Advent
open Day_23

let () =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  let start, finish = Trail_Map.(start trails, finish trails) in
  let hike = Option.get (Trail_Map.longest_hike trails start finish) in
  Format.(Trail_Map.pp ~highlight:(Pos_Set.to_list hike) err_formatter trails);
  print_endline (string_of_int (Pos_Set.cardinal hike))
