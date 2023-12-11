open Advent
open Day_07

let () =
  let total_winnings =
    List.of_seq (input stdin)
    |> List.map (fun (hand, bid) ->
           (* XXX: Don't sort by bid. *)
           (Hand.Freq.to_kind (Hand.to_freq hand), hand, bid))
    |> List.stable_sort compare
    |> List.mapi (fun i (_, _, bid) -> (i + 1) * bid)
    |> List.reduce ( + )
  in
  print_endline (string_of_int total_winnings)
