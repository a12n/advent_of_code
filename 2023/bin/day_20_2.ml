open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  let (Stats.{ low; _ } as stats) = Stats.make () in
  Config.pp Format.err_formatter cfg;
  Seq.(ints 1)
  |> Seq.find (fun i ->
         Format.(
           pp_print_int err_formatter i;
           pp_print_char err_formatter ':';
           pp_print_newline err_formatter ();
           Config.pp_state err_formatter cfg);
         Config.push_button cfg stats;
         Hashtbl.mem low "rx")
  |> Option.get |> string_of_int |> print_endline
