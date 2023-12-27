open Advent
open Day_22

let () =
  let bricks = Snapshot.of_lines (input_lines stdin) in
  Format.(
    pp_print_string err_formatter "Input:\n";
    Snapshot.pp err_formatter bricks;
    pp_print_string err_formatter "Grid Span:\n";
    Point.pp err_formatter (Point.of_tuple (Brick.size (Snapshot.grid_span bricks)));
    pp_print_newline err_formatter ());
  let bricks = Snapshot.sort bricks in
  Format.(
    pp_print_string err_formatter "Sorted:\n";
    Snapshot.pp err_formatter bricks)
