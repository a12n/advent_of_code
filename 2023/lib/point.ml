open Fun.Ops

module Make (Elt : sig
  type t

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
end) =
struct
  type t = { x : Elt.t; y : Elt.t; z : Elt.t }

  let zero = { x = Elt.zero; y = Elt.zero; z = Elt.zero }
  let map f p = { x = f p.x; y = f p.y; z = f p.z }
  let map2 f p q = { x = f p.x q.x; y = f p.y q.y; z = f p.z q.z }
  let add = map2 Elt.add
  let sub = map2 Elt.sub
  let mul_elt v x = map (Elt.mul x) v
  let of_tuple (x, y, z) = { x; y; z }
  let to_tuple { x; y; z } = (x, y, z)

  let of_string s =
    match
      String.split_on_char ',' s |> List.filter (( <> ) "") |> List.map (Elt.of_string % String.trim)
    with
    | [ x; y; z ] -> { x; y; z }
    | _ -> invalid_arg __FUNCTION__

  let to_string { x; y; z } =
    "(" ^ Elt.to_string x ^ ", " ^ Elt.to_string y ^ ", " ^ Elt.to_string z ^ ")"

  let pp fmt p = Format.pp_print_string fmt (to_string p)
end

include Make (struct
  include Int

  let of_string = int_of_string
end)
