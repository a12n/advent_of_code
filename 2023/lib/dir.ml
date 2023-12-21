type t = Up | Left | Right | Down

let to_char = function Up -> '^' | Left -> '<' | Right -> '>' | Down -> 'v'
let to_pos = function Up -> (-1, 0) | Left -> (0, -1) | Right -> (0, 1) | Down -> (1, 0)