type t = Up | Left | Right | Down

let neg = function Up -> Down | Left -> Right | Right -> Left | Down -> Up

let of_char = function
  | '^' -> Up
  | '<' -> Left
  | '>' -> Right
  | 'v' -> Down
  | _ -> invalid_arg __FUNCTION__

let to_char = function Up -> '^' | Left -> '<' | Right -> '>' | Down -> 'v'
let to_string = function Up -> "↑" | Left -> "←" | Down -> "↓" | Right -> "→"
let turn_left = function Up -> Left | Left -> Down | Down -> Right | Right -> Up
let turn_right = function Up -> Right | Left -> Up | Down -> Left | Right -> Down
