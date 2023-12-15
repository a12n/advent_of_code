open Advent
open Day_10

let () =
  let n =
    Grid.of_lines (input_lines stdin) |> Grid.cycles |> List.map List.length |> List.reduce Int.max
  in
  print_endline (string_of_int (n / 2))
