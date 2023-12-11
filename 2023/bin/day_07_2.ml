open Advent
open Day_07

let () =
  let joker = true in
  let total_winnings =
    List.of_seq (input stdin)
    |> List.map (fun (hand, bid) ->
           (Hand.Freq.to_kind ~joker (Hand.to_freq ~joker hand), hand, bid))
    |> List.stable_sort (fun (kind1, hand1, _) (kind2, hand2, _) ->
           let= _ = compare kind1 kind2 in
           Hand.compare ~joker hand1 hand2)
    |> List.mapi (fun i (_, _, bid) -> (i + 1) * bid)
    |> List.reduce ( + )
  in
  print_endline (string_of_int total_winnings)
