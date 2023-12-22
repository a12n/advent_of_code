open Advent
open Day_10

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  Grid.pp Format.err_formatter grid;
  let cycle = Option.get (Grid.cycle grid) in
  let pp_sep fmt () = Format.pp_print_string fmt "; " in
  Format.(
    pp_print_string err_formatter "Cycle: ";
    pp_print_list ~pp_sep Pos.pp Format.err_formatter cycle;
    pp_print_newline err_formatter ()
  );
  let cycle = List.rev (Polygon.compact cycle) in
  Format.(
    pp_print_string err_formatter "Compact Rev Cycle: ";
    pp_print_list ~pp_sep Pos.pp Format.err_formatter cycle;
    pp_print_newline err_formatter ()
  );
  Polygon.interior_area cycle |> string_of_int |> print_endline
