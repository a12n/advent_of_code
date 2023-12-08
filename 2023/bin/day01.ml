module String : sig
  include module type of Stdlib.String

  val to_list : string -> char list
end = struct
  include Stdlib.String

  let to_list s = List.init (length s) (unsafe_get s)
end

let ( let* ) = Option.bind

let rec next_digit = function
  | [] -> (None, [])
  | '1' :: s | 'o' :: 'n' :: 'e' :: s -> (Some 1, s)
  | '2' :: s | 't' :: 'w' :: 'o' :: s -> (Some 2, s)
  | '3' :: s | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: s -> (Some 3, s)
  | '4' :: s | 'f' :: 'o' :: 'u' :: 'r' :: s -> (Some 4, s)
  | '5' :: s | 'f' :: 'i' :: 'v' :: 'e' :: s -> (Some 5, s)
  | '6' :: s | 's' :: 'i' :: 'x' :: s -> (Some 6, s)
  | '7' :: s | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: s -> (Some 7, s)
  | '8' :: s | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: s -> (Some 8, s)
  | '9' :: s | 'n' :: 'i' :: 'n' :: 'e' :: s -> (Some 9, s)
  | _ :: s -> next_digit s

let rec digits chars =
  match next_digit chars with
  | Some d, chars' -> d :: digits chars'
  | None, _ -> []

let fist_and_last acc n =
  match acc with None -> Some (n, n) | Some (first, _) -> Some (first, n)

let calibration chars =
  let* first, last = List.fold_left fist_and_last None (digits chars) in
  Some ((first * 10) + last)

let total_calibration lines =
  Seq.map String.to_list lines
  |> Seq.filter_map calibration
  |> Seq.fold_left ( + ) 0

let () =
  let lines =
    Seq.of_dispenser (fun () ->
        match read_line () with l -> Some l | exception End_of_file -> None)
  in
  print_endline (string_of_int (total_calibration lines))
