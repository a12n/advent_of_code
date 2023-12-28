open Advent
open Day_05

let () =
  let (Almanac.{ seeds; _ } as almanac) = Almanac.of_lines (input_lines stdin) in
  let seeds =
    List.flatten (List.map (fun (s0, s1) -> Segment.[ of_length s0 0; of_length s1 0 ]) seeds)
  in
  let locations = Almanac.seed_to_location almanac seeds in
  let Segment.{ min = min_location; _ } = List.reduce Stdlib.min locations in
  print_endline (string_of_int min_location)
