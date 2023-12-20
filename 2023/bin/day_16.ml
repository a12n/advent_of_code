open Advent

module Dir = struct
  type t = Up | Left | Right | Down
end

module Mirror = struct
  type t = Upward | Downward

  let of_char = function '/' -> Upward | '\\' -> Downward | _ -> invalid_arg __FUNCTION__
  let to_char = function Upward -> '/' | Downward -> '\\'
end

module Splitter = struct
  type t = Horiz | Vert

  let of_char = function '-' -> Horiz | '|' -> Vert | _ -> invalid_arg __FUNCTION__
  let to_char = function Horiz -> '-' | Vert -> '|'
end

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end = struct
  type t = [ `Reflect of Mirror.t | `Split of Splitter.t ] option array array

  let opt_of_char = function
    | '.' -> None
    | c -> (
        try Some (`Reflect (Mirror.of_char c))
        with Invalid_argument _ -> Some (`Split (Splitter.of_char c)))

  let opt_to_char = function
    | None -> '.'
    | Some (`Reflect m) -> Mirror.to_char m
    | Some (`Split s) -> Splitter.to_char s

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map opt_of_char % String.to_seq)

  let pp fmt grid =
    let n_rows, n_cols = Array.matrix_size grid in
    Format.pp_print_newline fmt ();
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        Format.pp_print_char fmt (opt_to_char grid.(row).(col))
      done;
      Format.pp_print_newline fmt ()
    done
end
