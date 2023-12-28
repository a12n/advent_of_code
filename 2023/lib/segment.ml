type t = { min : int; max : int }

let empty = { min = 1; max = 0 }

let of_endpoints min max =
  if min > max then invalid_arg __FUNCTION__;
  { min; max }

let of_length min = function
  | 0 -> { min; max = min }
  | n when n > 0 -> { min; max = min + n - 1 }
  | _ -> invalid_arg __FUNCTION__

let length { min; max } = Int.max 0 (max - min + 1)
let is_disjoint s t = t.min > s.max || s.min > t.max
let is_empty { min; max } = min > max

let inter_opt s t =
  if is_disjoint s t then None else Some { min = Int.max s.min t.min; max = Int.min s.max t.max }

let union_opt s t =
  if is_disjoint s t then None else Some { min = Int.min s.min t.min; max = Int.max s.max t.max }

let inter s t = Option.get (inter_opt s t)
let union s t = Option.get (union_opt s t)

let diff s t =
  match inter_opt s t with
  | Some { min; max } ->
      let left_of_t = if min > s.min then [ { min = s.min; max = min - 1 } ] else [] in
      let right_of_t = if max < s.max then [ { min = max + 1; max = s.max } ] else [] in
      left_of_t @ right_of_t
  | None -> [ s ]

let partition k { min; max } = ({ min; max = Int.min (k - 1) max }, { min = Int.max k min; max })
