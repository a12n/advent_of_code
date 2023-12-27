open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  Seq.(ints 1)
  |> Seq.find (fun n ->
         ignore (Config.push_button cfg);
         ignore n;
         false)
  |> Option.get |> string_of_int |> print_endline
