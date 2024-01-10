open Fun.Ops

module type ELT = sig
  type t

  val zero : t
  val one : t
  val equal : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
end

module Make (Elt : ELT) = struct
  type t = { x : Elt.t; y : Elt.t; z : Elt.t }

  let zero = { x = Elt.zero; y = Elt.zero; z = Elt.zero }
  let unit_x = { zero with x = Elt.one }
  let unit_y = { zero with y = Elt.one }
  let unit_z = { zero with z = Elt.one }
  let map f p = { x = f p.x; y = f p.y; z = f p.z }
  let map2 f p q = { x = f p.x q.x; y = f p.y q.y; z = f p.z q.z }
  let reduce f p = f (f p.x p.y) p.z
  let dot u v = reduce Elt.add (map2 Elt.mul u v)
  let norm2 u = dot u u
  let add = map2 Elt.add
  let sub = map2 Elt.sub
  let mul_elt v x = map (Elt.mul x) v
  let of_tuple (x, y, z) = { x; y; z }
  let to_tuple { x; y; z } = (x, y, z)

  (** Intersection of two lines (rays) passing through the origin
      points to the directions. Result is a pair of numbers [t1] and
      [t2], such that [o1 + t1 * d1 = o2 + t2 * d2]. *)
  let intersect (o1, d1) (o2, d2) =
    (* https://paulbourke.net/geometry/pointlineplane/
       o1 = p1;
       o2 = p3;
       d1 = p2 - p1;
       d2 = p4 - p3; *)
    let ( + ) = Elt.add in
    let ( - ) = Elt.sub in
    let ( * ) = Elt.mul in
    let ( / ) = Elt.div in
    let ( let* ) = Option.bind in
    let non_zero x = if Elt.(equal x zero) then None else Some x in
    let* norm_d2 = non_zero (norm2 d2) in
    let dot_d2_d1 = dot d2 d1 in
    let* denom = non_zero ((norm2 d1 * norm_d2) - (dot_d2_d1 * dot_d2_d1)) in
    let diff = sub o1 o2 in
    let t1 = ((dot diff d2 * dot_d2_d1) - (dot diff d1 * norm_d2)) / denom in
    let t2 = (dot diff d2 + (t1 * dot_d2_d1)) / norm_d2 in
    Some (t1, t2)

  let of_string s =
    match
      String.split_on_char ',' s |> List.filter (( <> ) "") |> List.map (Elt.of_string % String.trim)
    with
    | [ x; y; z ] -> { x; y; z }
    | _ -> invalid_arg __FUNCTION__

  let to_string { x; y; z } =
    "(" ^ Elt.to_string x ^ ", " ^ Elt.to_string y ^ ", " ^ Elt.to_string z ^ ")"

  let pp fmt p = Format.pp_print_string fmt (to_string p)

  module Ops = struct
    let ( *. ) = mul_elt
    let ( + ) = add
    let ( - ) = sub
  end
end
