open Advent
open Day_15

let () =
  let total, hash =
    input_chars stdin
    |> Seq.fold_left
         (fun (total, hash) c ->
           match c with
           | '\n' -> (total, hash)
           | ',' -> (total + hash, 0)
           | c -> (total, update_hash hash c))
         (0, 0)
  in
  print_endline (string_of_int (total + hash))
