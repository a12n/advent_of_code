open Advent
open Day_24

let () =
  let hailstones = of_lines (input_lines stdin) in
  let min, max =
    Option.
      ( value ~default:Q.(of_int 200000000000000) (map Q.of_string (Sys.getenv_opt "MIN")),
        value ~default:Q.(of_int 400000000000000) (map Q.of_string (Sys.getenv_opt "MAX")) )
  in
  Printf.eprintf "%d hailstones, min %s, max %s\n%!" (List.length hailstones) (Q.to_string min)
    (Q.to_string max);
  List.combine_tl hailstones
  |> List.map (fun (((p0, v0) as h0), hs) ->
         List.filter_map
           (fun ((p1, v1) as h1) ->
             let* tx, ty = Hailstone.intersect2 h0 h1 in
             let p =
               Point.(Ops.(p0 + ({ zero with x = v0.x } *. tx) + ({ zero with y = v0.y } *. ty)))
             in
             Format.(
               Point.pp err_formatter p0;
               pp_print_string err_formatter " @ ";
               Vector.pp err_formatter v0;
               pp_print_string err_formatter "; ";
               Point.pp err_formatter p1;
               pp_print_string err_formatter " @ ";
               Vector.pp err_formatter v1;
               pp_print_string err_formatter " = ";
               Q.pp_print err_formatter tx;
               pp_print_string err_formatter ", ";
               Q.pp_print err_formatter ty;
               pp_print_string err_formatter "; p ";
               Point.pp err_formatter p;
               pp_print_newline err_formatter ());
             Some p)
           hs)
  |> List.concat
  |> List.filter (fun Point.{ x; y; _ } ->
         Format.(
           fprintf err_formatter "min %s, max %s; x %s, y %s\n%!" (Q.to_string min)
             (Q.to_string max) (Q.to_string x) (Q.to_string y));
         min <= x && x <= max && min <= y && y <= max)
  |> List.length |> string_of_int |> print_endline
