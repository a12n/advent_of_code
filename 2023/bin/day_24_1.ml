open Advent
open Day_24

let () =
  let hailstones = of_lines (input_lines stdin) in
  List.iter
    Format.(
      fun (p, v) ->
        pp_print_string err_formatter "p ";
        Point.pp err_formatter p;
        pp_print_string err_formatter ", v ";
        Vector.pp err_formatter v;
        pp_print_newline err_formatter ())
    hailstones
