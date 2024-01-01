open Advent
open Day_21

let () =
  let garden = Garden.of_lines (input_lines stdin) in
  let n = Option.(value ~default:64 (map int_of_string (Sys.getenv_opt "N"))) in
  let start = Option.get (Garden.start garden) in
  let reachable = Garden.steps garden start n in
  Format.(
    fprintf err_formatter "N = %d\n%!" n;
    Garden.pp ~highlight:reachable err_formatter garden);
  print_endline (string_of_int (List.length reachable))
