module Card : sig
  type t = private int

  val of_char : char -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val to_char : t -> char
end = struct
  type t = int

  let of_char = function
    | '2' -> 0
    | '3' -> 1
    | '4' -> 2
    | '5' -> 3
    | '6' -> 4
    | '7' -> 5
    | '8' -> 6
    | '9' -> 7
    | 'T' | 't' -> 8
    | 'J' | 'j' -> 9
    | 'Q' | 'q' -> 10
    | 'K' | 'k' -> 11
    | 'A' | 'a' -> 12
    | _ -> invalid_arg __FUNCTION__

  let to_char = String.unsafe_get "23456789TJQKA"
  let pp fmt c = Format.pp_print_char fmt (to_char c)
end

module Hand : sig
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  let of_string s =
    let open String in
    if length s <> 5 then invalid_arg __FUNCTION__;
    ( Card.of_char (unsafe_get s 0),
      Card.of_char (unsafe_get s 1),
      Card.of_char (unsafe_get s 2),
      Card.of_char (unsafe_get s 3),
      Card.of_char (unsafe_get s 4) )

  let to_string (c0, c1, c2, c3, c4) =
    let open Bytes in
    let buf = create 5 in
    unsafe_set buf 0 (Card.to_char c0);
    unsafe_set buf 1 (Card.to_char c1);
    unsafe_set buf 2 (Card.to_char c2);
    unsafe_set buf 3 (Card.to_char c3);
    unsafe_set buf 4 (Card.to_char c4);
    unsafe_to_string buf
end

let input chan =
  Seq.map
    (fun line ->
      match String.split_on_char ' ' line |> List.map String.trim with
      | [ hand; bid ] -> (Hand.of_string hand, int_of_string bid)
      | _ -> invalid_arg (__FUNCTION__ ^ ": invalid line \"" ^ line ^ "\""))
    (Advent.input_lines chan)
