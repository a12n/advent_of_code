open Advent
open Day_17
open Grid.Ops

let () =
  let grid = Heat_Grid.of_lines (input_lines stdin) in
  let n_rows, n_cols = Heat_Grid.size grid in
  let src, dest = ((0, 0), (n_rows - 1, n_cols - 1)) in
  let costs, dirs = Heat_Grid.distance grid src in
  let path = Heat_Grid.backtrack dirs dest in
  Format.(
    pp_print_string err_formatter "==== Heat-loss ====\n";
    Grid.pp ~highlight:path (fun fmt cost -> fprintf fmt " %3d" cost) err_formatter costs;
    pp_print_string err_formatter "==== Direction ====\n";
    Grid.pp ~highlight:path
      (fun fmt dir ->
        fprintf fmt " %s" (match dir with None -> "  ." | Some dir -> "  " ^ Dir.to_string dir))
      err_formatter dirs;
    pp_print_string err_formatter "==== Grid ====\n";
    Heat_Grid.pp ~path Format.err_formatter grid);
  print_endline (string_of_int costs.@(dest))
