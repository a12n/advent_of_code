type t = Up | Left | Right | Down

let neg = function Up -> Down | Left -> Right | Right -> Left | Down -> Up
let to_char = function Up -> '^' | Left -> '<' | Right -> '>' | Down -> 'v'

(* TODO: Move to Pos.of_dir *)
let to_pos = function Up -> (-1, 0) | Left -> (0, -1) | Right -> (0, 1) | Down -> (1, 0)
let to_string = function Up -> "↑" | Left -> "←" | Down -> "↓" | Right -> "→"

let of_pos = function
  | 0, n when n < 0 -> Left
  | 0, n when n > 0 -> Right
  | n, 0 when n < 0 -> Up
  | n, 0 when n > 0 -> Down
  | _, _ -> invalid_arg __FUNCTION__

let turn_left = function Up -> Left | Left -> Down | Down -> Right | Right -> Up
let turn_right = function Up -> Right | Left -> Up | Down -> Left | Right -> Down
