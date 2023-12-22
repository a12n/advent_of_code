open Advent
open Day_10

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  Grid.pp Format.err_formatter grid;
  let cycle = Option.get (Grid.cycle grid) in
  Printf.eprintf "cycle %d\n%!" (List.length cycle);
  let cycle = Polygon.compact cycle in
  Printf.eprintf "compact cycle %d\n%!" (List.length cycle);
  Polygon.area cycle |> string_of_int |> print_endline
