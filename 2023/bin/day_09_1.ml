open Advent

let rev_diffs = List.diffs (Fun.flip ( - ))

let rec extrapolate xs =
  if List.for_all (( = ) 0) xs then 0
  else
    let ys = rev_diffs xs in
    List.hd xs + extrapolate ys

let () =
  let sum  =
    input_lines stdin
    |> Seq.map (String.split_on_char ' ')
    |> Seq.map (List.filter (( <> ) ""))
    |> Seq.map (List.map int_of_string)
    |> Seq.map List.rev
    |> Seq.map extrapolate
    |> Seq.reduce (+)
  in
  print_endline (string_of_int sum)
