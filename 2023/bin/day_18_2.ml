open Advent
open Day_18

let () =
  (* FIXME: Must include interior_area and boundary_area. As for now
     interior_area erroneously includes boundary area in the result. *)
  Plan.of_lines2 (input_lines stdin)
  |> Plan.to_list_pos |> Polygon.interior_area |> string_of_int |> print_endline
