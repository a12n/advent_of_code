open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  Config.pp Format.err_formatter cfg;
  let Stats.{ low; high } =
    Seq.(ints 1)
    |> Seq.take 1000
    |> Seq.fold_left
         (fun total _i ->
           let _, stats = Config.push_button cfg in
           Stats.add total stats)
         Stats.zero
  in
  print_endline (string_of_int (low * high))
