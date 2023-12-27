open Advent
open Day_21

let () =
  let garden = Garden.of_lines (input_lines stdin) in
  let reachable = Garden.steps garden (Option.get (Garden.start garden)) 64 in
  Garden.pp ~highlight:reachable Format.err_formatter garden
