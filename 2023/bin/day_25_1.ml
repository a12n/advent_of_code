open Advent
open Day_25

let () =
  let conns = of_lines (input_lines stdin) in
  Format.(
    let fmt = err_formatter in
    pp_print_string fmt "graph {\n";
    Hashtbl.iter (fun k v -> fprintf fmt "\t%s -- %s;\n" k v) conns;
    pp_print_string fmt "}\n")
