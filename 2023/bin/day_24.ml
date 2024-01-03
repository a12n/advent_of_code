open Advent
module Vector = Point

module Hailstone = struct
  type t = Point.t * Vector.t

  let of_string s =
    match String.split_on_char '@' s |> List.map String.trim with
    | [ p; v ] -> (Point.of_string p, Vector.of_string v)
    | _ -> invalid_arg __FUNCTION__

  let intersect (p1, v1) (p2, v2) =
    (* TODO *)
    ignore (p1, v1);
    ignore (p2, v2);
    None
end

let of_lines = List.of_seq % Seq.map Hailstone.of_string
