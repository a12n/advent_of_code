open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  Config.pp Format.err_formatter cfg;
  Config.run cfg Pulse.Low "broadcaster" 0
