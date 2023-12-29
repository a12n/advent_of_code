open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  let stats = Stats.make () in
  Config.pp Format.err_formatter cfg;
  Seq.(ints 1) |> Seq.take 1000 |> Seq.iter (fun _i -> Config.push_button cfg stats);
  let low, high = Stats.total stats in
  print_endline (string_of_int (low * high))
