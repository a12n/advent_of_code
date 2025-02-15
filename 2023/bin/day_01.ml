open Advent

let ( let* ) = Option.bind

let rec next_digit = function
  | [] -> (None, [])
  | '1' :: s | 'o' :: ('n' :: 'e' :: _ as s) -> (Some 1, s)
  | '2' :: s | 't' :: ('w' :: 'o' :: _ as s) -> (Some 2, s)
  | '3' :: s | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as s) -> (Some 3, s)
  | '4' :: s | 'f' :: ('o' :: 'u' :: 'r' :: _ as s) -> (Some 4, s)
  | '5' :: s | 'f' :: ('i' :: 'v' :: 'e' :: _ as s) -> (Some 5, s)
  | '6' :: s | 's' :: ('i' :: 'x' :: _ as s) -> (Some 6, s)
  | '7' :: s | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as s) -> (Some 7, s)
  | '8' :: s | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as s) -> (Some 8, s)
  | '9' :: s | 'n' :: ('i' :: 'n' :: 'e' :: _ as s) -> (Some 9, s)
  | _ :: s -> next_digit s

let rec digits chars =
  match next_digit chars with
  | Some d, chars' -> d :: digits chars'
  | None, _ -> []

let fist_and_last acc n =
  match acc with None -> Some (n, n) | Some (first, _) -> Some (first, n)

let calibration_value chars =
  let* first, last = List.fold_left fist_and_last None (digits chars) in
  Some ((first * 10) + last)

let total_calibration_value lines =
  Seq.map String.to_list lines
  |> Seq.filter_map calibration_value
  |> Seq.fold_left ( + ) 0
