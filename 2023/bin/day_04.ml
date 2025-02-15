open Advent

module Numbers : sig
  include module type of Set.Make (Int)

  val of_string : string -> t
end = struct
  include Set.Make (Int)

  let of_string s =
    String.split_on_char ' ' s
    |> List.filter (( <> ) "")
    |> List.map int_of_string |> of_list
end

module Card : sig
  type t = int * Numbers.t

  val of_string : string -> t
end = struct
  type t = int * Numbers.t

  let of_string s =
    match String.split_on_char ':' s with
    | [ header; numbers ] -> (
        match String.split_on_char ' ' header |> List.filter (( <> ) "") with
        | [ "Card"; id ] -> (
            match String.split_on_char '|' numbers with
            | [ winning; have ] ->
                ( int_of_string id,
                  Numbers.(inter (of_string winning) (of_string have)) )
            | _ -> invalid_arg __FUNCTION__)
        | _ -> invalid_arg __FUNCTION__)
    | _ -> invalid_arg __FUNCTION__
end
