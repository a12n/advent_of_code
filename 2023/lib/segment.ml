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
let is_disjoint s1 s2 = s2.min > s1.max || s1.min > s2.max
let is_empty { min; max } = min > max

let inter_opt s1 s2 =
  if is_disjoint s1 s2 then None
  else Some { min = Int.max s1.min s2.min; max = Int.min s1.max s2.max }

let union_opt s1 s2 =
  if is_empty s1 then Some s2
  else if is_empty s2 then Some s1
  else if is_disjoint s1 s2 then
    if s1.max + 1 = s2.min then Some { min = s1.min; max = s2.max }
    else if s2.max + 1 = s1.min then Some { min = s2.min; max = s1.max }
    else None
  else Some { min = Int.min s1.min s2.min; max = Int.max s1.max s2.max }

let inter s1 s2 = match inter_opt s1 s2 with Some s -> s | None -> invalid_arg __FUNCTION__
let union s1 s2 = match union_opt s1 s2 with Some s -> s | None -> invalid_arg __FUNCTION__

let diff s t =
  match inter_opt s t with
  | Some { min; max } ->
      let left_of_t = if min > s.min then [ { min = s.min; max = min - 1 } ] else [] in
      let right_of_t = if max < s.max then [ { min = max + 1; max = s.max } ] else [] in
      left_of_t @ right_of_t
  | None -> [ s ]

let partition k { min; max } = ({ min; max = Int.min (k - 1) max }, { min = Int.max k min; max })

let pp fmt s =
  if is_empty s then Format.pp_print_string fmt "∅"
  else if s.min = s.max then Format.pp_print_int fmt s.min
  else Format.fprintf fmt "[%d, %d]" s.min s.max
