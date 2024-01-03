open Advent
open Day_24

let () =
  of_lines (input_lines stdin)
  |> List.combine_tl
  |> List.map (fun (h0, hs) -> List.filter_map (Hailstone.intersect h0) hs)
  |> List.concat |> List.length |> string_of_int |> print_endline
