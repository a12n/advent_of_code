open Advent
open Day_11

let () =
  Image.of_chars (input_chars stdin)
  |> Image.expand |> List.combine_tl
  |> List.map (fun (g, gs) -> List.map (Image.dist g) gs)
  |> List.concat |> List.reduce ( + ) |> string_of_int |> print_endline
