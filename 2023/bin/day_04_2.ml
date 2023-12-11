open Advent
open Day_04

let () =
  let matching =
    input_lines stdin |> Seq.map Card.of_string
    |> Seq.mapi (fun i (id, matching) ->
           assert (i = id - 1);
           Numbers.cardinal matching)
    |> Array.of_seq
  in
  let n = Array.length matching in
  let copies = Array.make n 1 in
  for i = 0 to n - 1 do
    for j = 1 to matching.(i) do
      copies.(i + j) <- copies.(i + j) + copies.(i)
    done
  done;
  print_endline (string_of_int (Array.reduce ( + ) copies))
