open Advent

let rev_diffs = List.diffs (Fun.flip ( - ))

let rec rev_extrapolate l =
  if List.for_all (( = ) 0) l then 0
  else List.hd l + rev_extrapolate (rev_diffs l)

let () =
  let sum =
    input_lines stdin
    |> Seq.map (String.split_on_char ' ')
    |> Seq.map (List.filter (( <> ) ""))
    |> Seq.map (List.map int_of_string)
    |> Seq.map rev_extrapolate |> Seq.reduce ( + )
  in
  print_endline (string_of_int sum)
