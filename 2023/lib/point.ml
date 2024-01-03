open Fun.Ops

type t = { x : int; y : int; z : int }

let zero = { x = 0; y = 0; z = 0 }
let map2 f p q = { x = f p.x q.x; y = f p.y q.y; z = f p.z q.z }
let add = map2 ( + )
let sub = map2 ( - )
let of_tuple (x, y, z) = { x; y; z }
let to_tuple { x; y; z } = (x, y, z)

let of_string s =
  match
    String.split_on_char ',' s |> List.filter (( <> ) "") |> List.map (int_of_string % String.trim)
  with
  | [ x; y; z ] -> { x; y; z }
  | _ -> invalid_arg __FUNCTION__

let pp fmt { x; y; z } = Format.fprintf fmt "(%d, %d, %d)" x y z
