open Advent
open Day_21

let () =
  let garden = Garden.of_lines (input_lines stdin) in
  Garden.pp Format.err_formatter garden
