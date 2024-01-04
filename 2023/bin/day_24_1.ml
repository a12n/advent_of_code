open Advent
open Day_24

let () =
  let hailstones = of_lines (input_lines stdin) in
  let min_value, max_value =
    Option.
      ( value ~default:Q.(of_int 200000000000000) (map Q.of_string (Sys.getenv_opt "MIN")),
        value ~default:Q.(of_int 400000000000000) (map Q.of_string (Sys.getenv_opt "MAX")) )
  in
  Printf.eprintf "%d hailstones, min %s, max %s\n%!" (List.length hailstones)
    (Q.to_string min_value) (Q.to_string max_value);
  List.combine_tl hailstones
  |> List.map (fun (((p0, v0) as h0), hs) ->
         List.filter_map
           (fun ((p1, v1) as h1) ->
             let* x, y = Hailstone.intersect2 h0 h1 in
             Format.(
               Point.pp err_formatter p0;
               pp_print_string err_formatter " @ ";
               Vector.pp err_formatter v0;
               pp_print_string err_formatter "; ";
               Point.pp err_formatter p1;
               pp_print_string err_formatter " @ ";
               Vector.pp err_formatter v1;
               pp_print_string err_formatter " = ";
               pp_print_float err_formatter (Q.to_float x);
               pp_print_string err_formatter ", ";
               pp_print_float err_formatter (Q.to_float y);
               pp_print_newline err_formatter ());
             Some (x, y))
           hs)
  |> List.concat
  |> List.filter (fun (x, y) -> Q.(x >= min_value && x <= max_value && y >= min_value && y <= max_value))
  |> List.map (fun (x, y) ->
         Format.(fprintf err_formatter "pass (%f, %f)\n%!" (Q.to_float x) (Q.to_float y));
         (x, y))
  |> List.length |> string_of_int |> print_endline
