open Advent
open Day_18

let () =
  let points = Plan.of_lines2 (input_lines stdin) |> Plan.to_list_pos |> List.rev in
  print_endline (string_of_int Polygon.(interior_area points + boundary_area points))
