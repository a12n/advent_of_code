open Advent

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

  val compare : ?joker:bool -> t -> t -> int
  val of_char : char -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val to_char : t -> char
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

  let compare ?(joker = false) a b =
    match (joker, a, b) with
    | true, Jack, Jack -> 0
    | true, Jack, _ -> -1
    | true, _, Jack -> 1
    | _, _, _ -> Stdlib.compare a b

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

  let to_char = function
    | Two -> '2'
    | Three -> '3'
    | Four -> '4'
    | Five -> '5'
    | Six -> '6'
    | Seven -> '7'
    | Eight -> '8'
    | Nine -> '9'
    | Ten -> 'T'
    | Jack -> 'J'
    | Queen -> 'Q'
    | King -> 'K'
    | Ace -> 'A'

  let pp fmt c = Format.pp_print_char fmt (to_char c) [@@ocaml.toplevel_printer]
end

module Hand : sig
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  module Kind : sig
    type t =
      | High_Card
      | One_Pair
      | Two_Pair
      | Three_Of
      | Full_House
      | Four_Of
      | Five_Of
  end

  module Freq : sig
    type t = private (Card.t * int) list
    (** Frequency list of cards in a hand. *)

    val to_kind : ?joker:bool -> t -> Kind.t
  end

  val compare : ?joker:bool -> t -> t -> int
  val of_string : string -> t
  val to_freq : ?joker:bool -> t -> Freq.t
  val to_string : t -> string
end = struct
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  module Kind = struct
    type t =
      | High_Card
      | One_Pair
      | Two_Pair
      | Three_Of
      | Full_House
      | Four_Of
      | Five_Of
  end

  module Freq = struct
    type t = (Card.t * int) list

    let rec to_kind ?(joker = false) =
      if joker then function
        | [ (_, 1); (Card.Jack, 4) ] -> Kind.Five_Of
        | [ (Card.Jack, 1); (_, 4) ] -> Kind.Five_Of
        | [ (_, 2); (Card.Jack, 3) ] -> Kind.Five_Of
        | [ (Card.Jack, 2); (_, 3) ] -> Kind.Five_Of
        | [ (_, 1); (_, 1); (Card.Jack, 3) ] -> Kind.Four_Of
        | [ (Card.Jack, 1); (_, 1); (_, 3) ] -> Kind.Four_Of
        | [ (_, 1); (Card.Jack, 2); (_, 2) ] -> Kind.Four_Of
        | [ (Card.Jack, 1); (_, 2); (_, 2) ] -> Kind.Full_House
        | [ (_, 1); (_, 1); (_, 1); (Card.Jack, 2) ] -> Kind.Three_Of
        | [ (Card.Jack, 1); (_, 1); (_, 1); (_, 2) ] -> Kind.Three_Of
        | [ (Card.Jack, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> Kind.One_Pair
        | h -> to_kind ~joker:false h
      else function
        | [ (_, 5) ] -> Kind.Five_Of
        | [ (_, 1); (_, 4) ] -> Kind.Four_Of
        | [ (_, 2); (_, 3) ] -> Kind.Full_House
        | [ (_, 1); (_, 1); (_, 3) ] -> Kind.Three_Of
        | [ (_, 1); (_, 2); (_, 2) ] -> Kind.Two_Pair
        | [ (_, 1); (_, 1); (_, 1); (_, 2) ] -> Kind.One_Pair
        | _ -> Kind.High_Card
  end

  let compare ?(joker = false) (a0, a1, a2, a3, a4) (b0, b1, b2, b3, b4) =
    let= _ = Card.compare ~joker a0 b0 in
    let= _ = Card.compare ~joker a1 b1 in
    let= _ = Card.compare ~joker a2 b2 in
    let= _ = Card.compare ~joker a3 b3 in
    Card.compare ~joker a4 b4

  let to_freq ?(joker = false) (h0, h1, h2, h3, h4) =
    List.fold_left
      (fun freq card ->
        let n = Option.value ~default:0 (List.assoc_opt card freq) in
        let freq' = List.remove_assoc card freq in
        (card, n + 1) :: freq')
      [ (h0, 1) ]
      [ h1; h2; h3; h4 ]
    |> List.sort (fun (card1, n1) (card2, n2) ->
           match Int.compare n1 n2 with
           | 0 -> Card.compare ~joker card1 card2
           | ord -> ord)

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

let run ~joker =
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
