open Advent
open Day_23

let () =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  let start, finish = Trail_Map.(start trails, finish trails) in
  let g = Trail_Map.to_graph trails in
  Trail_Map.longest_path g start finish |> Option.get |> string_of_int |> print_endline
