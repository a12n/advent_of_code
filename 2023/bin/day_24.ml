open Advent
module Point = Point.Rational
module Vector = Point

let ( let* ) = Option.bind
let intersect (p1, v1) (p2, v2) = if Q.(v1 <> v2) then Some Q.((p2 - p1) / (v1 - v2)) else None

module Hailstone = struct
  type t = Point.t * Vector.t

  let of_string s =
    match String.split_on_char '@' s |> List.map String.trim with
    | [ p; v ] -> (Point.of_string p, Vector.of_string v)
    | _ -> invalid_arg __FUNCTION__

  let intersect2 ((p1, v1) : t) ((p2, v2) : t) =
    let* tx = intersect Point.(p1.x, v1.x) Point.(p2.x, v2.x) in
    let* ty = intersect Point.(p1.y, v1.y) Point.(p2.y, v2.y) in
    if Q.(tx > zero && ty > zero) then Some (tx, ty) else None
end

let of_lines = List.of_seq % Seq.map Hailstone.of_string
