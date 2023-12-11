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

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = Card.t * Card.t * Card.t * Card.t * Card.t

  let to_list (h0, h1, h2, h3, h4) = (h0, [ h1; h2; h3; h4 ])

  module Freq = struct
    let of_hand (h0, h1, h2, h3, h4) =
      let freq = Array.make 13 0 in
      List.iter
        (fun c ->
          let i = Card.to_int c in
          Array.(unsafe_set freq i (unsafe_get freq i + 1)))
        [ h0; h1; h2; h3; h4 ];
      freq
  end

  module Kind = struct
    let five_of_a_kind h = Array.exists (( = ) 5) (Freq.of_hand h)
    let four_of_a_kind h = Array.exists (( = ) 4) (Freq.of_hand h)

    let full_house h =
      let freq = Freq.of_hand h in
      Array.exists (( = ) 3) freq && Array.exists (( = ) 2) freq

    let three_of_a_kind _ =
      (* TODO *)
      false

    let two_pair _ =
      (* TODO *)
      false

    let one_pair _ =
      (* TODO *)
      false

    let high_card _ =
      (* TODO *)
      false
  end

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
