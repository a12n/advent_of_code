open Advent
open Day_05

let () =
  let (Almanac.{ seeds; _ } as almanac) =
    Almanac.of_lines (input_lines stdin)
  in
  let min_location, _seed =
    List.unpair seeds
    |> List.map (fun seed -> (Almanac.seed_to_location almanac seed, seed))
    |> List.fold_left min (max_int, 0)
  in
  print_endline (string_of_int min_location)
