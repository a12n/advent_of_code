type t = Up | Left | Right | Down

let neg = function Up -> Down | Left -> Right | Right -> Left | Down -> Up
let to_char = function Up -> '^' | Left -> '<' | Right -> '>' | Down -> 'v'
let to_pos = function Up -> (-1, 0) | Left -> (0, -1) | Right -> (0, 1) | Down -> (1, 0)
let to_string = function Up -> "↑" | Left -> "←" | Down -> "↓" | Right -> "→"
