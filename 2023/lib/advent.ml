(* Stdlib extensions *)
module Array = Advent__Array
module Int = Advent__Int
module List = Advent__List
module Seq = Advent__Seq
module String = Advent__String

(* Shared modules *)
module Algorithm = Advent__Algorithm
module Dir = Advent__Dir
module Polygon = Advent__Polygon
module Pos = Advent__Pos

let ( % ) f g x = f (g x)

(** Binding operator to short-circuit chains of [compare] calls for
    tuples. *)
let ( let= ) ord f = if ord <> 0 then ord else f ord

let input_chars, input_lines =
  let input_seq read ch =
    (* XXX: The module Seq is an alias for module Advent__Seq, which
       is missing. *)
    Seq.of_dispenser (fun () -> match read ch with x -> Some x | exception End_of_file -> None)
  in
  (input_seq input_char, input_seq input_line)
