open Advent
open Day_18

let () =
  let plan = Plan.of_lines (input_lines stdin) in
  let grid = Grid.of_plan plan in
  Format.(
    pp_print_string Format.err_formatter "Before:\n";
    Grid.pp err_formatter grid;
  );
  Grid.dig_interior grid;
  Format.(
    pp_print_string Format.err_formatter "After:\n";
    Grid.pp err_formatter grid;
  )
