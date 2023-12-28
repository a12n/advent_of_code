open Advent
open Day_05

let () =
  let (Almanac.{ seeds; _ } as almanac) = Almanac.of_lines (input_lines stdin) in
  let seeds = List.map (fun (min, n) -> Segment.of_length min n) seeds in
  let locations = Almanac.seed_to_location almanac seeds in
  let Segment.{ min = min_location; _ } = List.reduce Stdlib.min locations in
  print_endline (string_of_int min_location)
