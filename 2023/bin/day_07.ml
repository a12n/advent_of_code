open Advent

module Card : sig
  type t = private int

  val of_char : char -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val to_string : t -> string
end = struct
  type t = int

  let of_char = function
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' | 't' -> 10
    | 'J' | 'j' -> 11
    | 'Q' | 'q' -> 12
    | 'K' | 'k' -> 13
    | 'A' | 'a' -> 14
    | _ -> invalid_arg __FUNCTION__

  let to_string c =
    Array.(
      unsafe_get
        [| "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "J"; "Q"; "K"; "A" |]
        (c - 2))

  let pp fmt c = Format.pp_print_string fmt (to_string c)
end

let part1 () = print_endline (string_of_int 0)
let part2 () = print_endline (string_of_int 0)
let () = (parse_args Sys.argv [| part1; part2 |]) ()
