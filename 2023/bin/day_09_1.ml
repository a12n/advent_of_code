open Advent

let rev_diffs = List.diffs (Fun.flip ( - ))

let rec rev_extrapolate l =
  if List.for_all (( = ) 0) l then 0
  else List.hd l + rev_extrapolate (rev_diffs l)

let () =
  Day_09.input stdin
  |> Seq.map List.rev
  |> Seq.map rev_extrapolate
  |> Seq.reduce ( + )
  |> string_of_int
  |> print_endline
