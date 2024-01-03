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
         List.filter_map (Hailstone.intersect2 h0) hs
         |> List.map (fun (tx, ty) ->
                let p = Point.(Ops.(p0 + (unit_x *. tx) + (unit_y *. ty))) in
                Format.(
                  pp_print_string err_formatter "p0 ";
                  Point.pp err_formatter p0;
                  pp_print_string err_formatter ", v0 ";
                  Vector.pp err_formatter v0;
                  pp_print_string err_formatter ", tx ";
                  Q.pp_print err_formatter tx;
                  pp_print_string err_formatter ", ty ";
                  Q.pp_print err_formatter ty;
                  pp_print_string err_formatter ", p ";
                  Point.pp err_formatter p;
                  pp_print_newline err_formatter ());
                p))
  |> List.concat
  |> List.filter (fun Point.{ x; y; _ } -> min <= x && x <= max && min <= y && y <= max)
  |> List.length |> string_of_int |> print_endline
