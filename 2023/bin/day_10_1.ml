open Advent
open Day_10

let () =
  let n = Grid.(cycle (of_lines (input_lines stdin))) |> Option.get |> List.length in
  print_endline (string_of_int (n / 2))
