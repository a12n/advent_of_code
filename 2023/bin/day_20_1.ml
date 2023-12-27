open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  Config.pp Format.err_formatter cfg;
  let Stats.{ low; high } = Config.push_button cfg in
  Printf.printf "low %d, high %d\n" low high
