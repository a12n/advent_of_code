open Advent
open Day_21

let () =
  let garden = Garden.of_lines (input_lines stdin) in
  let n = Option.(value ~default:26501365 (map int_of_string (Sys.getenv_opt "N"))) in
  Format.(
    fprintf err_formatter "N = %d\n%!" n;
    Garden.pp err_formatter garden)
