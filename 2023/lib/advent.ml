module List = Advent__List
module Seq = Advent__Seq
module String = Advent__String

let input_chars, input_lines =
  let input_seq read ch =
    Seq.of_dispenser (fun () ->
        match read ch with x -> Some x | exception End_of_file -> None)
  in
  (input_seq input_char, input_seq input_line)
