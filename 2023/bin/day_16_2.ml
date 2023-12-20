open Advent
open Day_16

let () =
  let grid = Grid.of_lines (input_lines stdin) in
  let n_rows, n_cols = Grid.size grid in
  List.(
    init n_cols (fun col -> (Dir.Down, (0, col)))
    @ init n_cols (fun col -> (Dir.Up, (n_rows - 1, col)))
    @ init n_rows (fun row -> (Dir.Right, (row, 0)))
    @ init n_rows (fun row -> (Dir.Left, (row, n_cols - 1))))
  |> List.map (fun (dir, pos) -> Grid.trace grid dir pos |> Energized_Map.length)
  |> List.reduce Int.max |> string_of_int |> print_endline
