module Array = Advent__Array
module Int = Advent__Int
module List = Advent__List
module Seq = Advent__Seq
module String = Advent__String

let input_chars, input_lines =
  let input_seq read ch =
    Seq.of_dispenser (fun () ->
        match read ch with x -> Some x | exception End_of_file -> None)
  in
  (input_seq input_char, input_seq input_line)

(** Choose one of the elements based on the index in args. *)
let parse_args args elts =
  let i = if Array.length args > 1 then int_of_string args.(1) - 1 else 0 in
  if i < Array.length elts then elts.(i) else invalid_arg __FUNCTION__
