module Card : sig
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

  val of_char : char -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val to_char : t -> char
  val to_int : t -> int
end = struct
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

  let of_char = function
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' | 't' -> Ten
    | 'J' | 'j' -> Jack
    | 'Q' | 'q' -> Queen
    | 'K' | 'k' -> King
    | 'A' | 'a' -> Ace
    | _ -> invalid_arg __FUNCTION__

  let to_int = function
    | Two -> 0
    | Three -> 1
    | Four -> 2
    | Five -> 3
    | Six -> 4
    | Seven -> 5
    | Eight -> 6
    | Nine -> 7
    | Ten -> 8
    | Jack -> 9
    | Queen -> 10
    | King -> 11
    | Ace -> 12

  let to_char c = String.unsafe_get "23456789TJQKA" (to_int c)
  let pp fmt c = Format.pp_print_char fmt (to_char c) [@@ocaml.toplevel_printer]
end

module Hand : sig
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  module Freq : sig
    type t = private (Card.t * int) list
    (** Frequency list of cards in a hand. *)

    val five_of_a_kind : t -> bool
    val four_of_a_kind : t -> bool
    val full_house : t -> bool
    val three_of_a_kind : t -> bool
    val two_pair : t -> bool
    val one_pair : t -> bool
    val high_card : t -> bool
  end

  val of_string : string -> t
  val to_freq : t -> Freq.t
  val to_string : t -> string
end = struct
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  let to_list (h0, h1, h2, h3, h4) = (h0, [ h1; h2; h3; h4 ])

  module Freq = struct
    type t = (Card.t * int) list

    let five_of_a_kind = function [ (_, 5) ] -> true | _ -> false
    let four_of_a_kind = function [ (_, 4); (_, 1) ] -> true | _ -> false
    let full_house = function [ (_, 3); (_, 2) ] -> true | _ -> true

    let three_of_a_kind = function
      | [ (_, 3); (_, 1); (_, 1) ] -> true
      | _ -> false

    let two_pair = function [ (_, 2); (_, 2); (_, 1) ] -> true | _ -> false

    let one_pair = function
      | [ (_, 2); (_, 1); (_, 1); (_, 1) ] -> true
      | _ -> false

    let high_card = function
      | [ (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> true
      | _ -> false
  end

  let to_freq (h0, h1, h2, h3, h4) =
    List.fold_left
      (fun freq card ->
        let n = Option.value ~default:0 (List.assoc_opt card freq) in
        let freq' = List.remove_assoc card freq in
        (card, n + 1) :: freq')
      [ (h0, 1) ]
      [ h1; h2; h3; h4 ]
    |> List.sort (fun (card1, n1) (card2, n2) ->
           match Int.compare n2 n1 with 0 -> compare card2 card1 | ord -> ord)

  let of_string s =
    let open Card in
    let open String in
    if length s <> 5 then invalid_arg __FUNCTION__;
    ( of_char (unsafe_get s 0),
      of_char (unsafe_get s 1),
      of_char (unsafe_get s 2),
      of_char (unsafe_get s 3),
      of_char (unsafe_get s 4) )

  let to_string (c0, c1, c2, c3, c4) =
    let open Bytes in
    let open Card in
    let b = create 5 in
    unsafe_set b 0 (to_char c0);
    unsafe_set b 1 (to_char c1);
    unsafe_set b 2 (to_char c2);
    unsafe_set b 3 (to_char c3);
    unsafe_set b 4 (to_char c4);
    unsafe_to_string b
end

let input chan =
  Seq.map
    (fun line ->
      match String.split_on_char ' ' line |> List.map String.trim with
      | [ hand; bid ] -> (Hand.of_string hand, int_of_string bid)
      | _ -> invalid_arg (__FUNCTION__ ^ ": invalid line \"" ^ line ^ "\""))
    (Advent.input_lines chan)
