open Advent

let diffs = List.diffs ( - )

let rec extrapolate l =
  if List.for_all (( = ) 0) l then 0 else List.hd l - extrapolate (diffs l)

let () =
  Day_09.input stdin
  |> Seq.map extrapolate
  |> Seq.reduce ( + )
  |> string_of_int
  |> print_endline
