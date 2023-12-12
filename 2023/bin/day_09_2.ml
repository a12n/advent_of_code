open Advent

let diffs = List.diffs ( - )

let rec extrapolate l =
  if List.for_all (( = ) 0) l then 0 else List.hd l - extrapolate (diffs l)

let () =
  let sum =
    Day_09.input stdin
    |> Seq.map extrapolate
    |> Seq.reduce ( + ) in
  print_endline (string_of_int sum)
