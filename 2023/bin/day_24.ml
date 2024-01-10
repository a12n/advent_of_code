open Advent
module Point = Point.Make (Q)
module Vector = Point

let ( let* ) = Option.bind
let intersect (p1, v1) (p2, v2) = if Q.(v1 <> v2) then Some Q.((p2 - p1) / (v1 - v2)) else None

module Hailstone = struct
  type t = Point.t * Vector.t

  let of_string s =
    match String.split_on_char '@' s |> List.map String.trim with
    | [ p; v ] -> (Point.of_string p, Vector.of_string v)
    | _ -> invalid_arg __FUNCTION__

  let intersect2 (p1, v1) (p2, v2) =
    let Point.{ x = x11; y = y11; _ } = p1 in
    let Point.{ x = x12; y = y12; _ } = Point.add p1 v1 in
    let Point.{ x = x21; y = y21; _ } = p2 in
    let Point.{ x = x22; y = y22; _ } = Point.add p2 v2 in
    let denom = Q.(((x11 - x12) * (y21 - y22)) - ((y11 - y12) * (x21 - x22))) in
    if Q.(denom <> zero) then
      let x =
        Q.(
          (((x11 * y12) - (y11 * x12)) * (x21 - x22)) - ((x11 - x12) * ((x21 * y22) - (y21 * x22))))
      in
      let y =
        Q.(
          (((x11 * y12) - (y11 * x12)) * (y21 - y22)) - ((y11 - y12) * ((x21 * y22) - (y21 * x22))))
      in
      Some Q.(x / denom, y / denom)
    else None

  let pp fmt (p, v) =
    Format.(
      pp_print_char fmt '(';
      Point.pp fmt p;
      pp_print_char fmt '@';
      Vector.pp fmt v;
      pp_print_char fmt ')')
end

let of_lines = List.of_seq % Seq.map Hailstone.of_string
