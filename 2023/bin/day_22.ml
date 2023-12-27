open Advent

module Point = struct
  type t = { x : int; y : int; z : int }

  let of_string s =
    match
      String.split_on_char ',' s |> List.filter (( <> ) "") |> List.map (int_of_string % String.trim)
    with
    | [ x; y; z ] -> { x; y; z }
    | _ -> invalid_arg __FUNCTION__

  let pp fmt { x; y; z } = Format.fprintf fmt "(%d, %d, %d)" x y z
end

module Brick = struct
  type t = Point.t * Point.t

  let of_string s =
    match String.split_on_char '~' s with
    | [ min; max ] -> (Point.of_string min, Point.of_string max)
    | _ -> invalid_arg __FUNCTION__

  let pp fmt (min, max) =
    Format.(
      pp_print_char fmt '(';
      Point.pp fmt min;
      pp_print_string fmt ", ";
      Point.pp fmt max;
      pp_print_char fmt ')')
end

let input chan = List.of_seq (Seq.map Brick.of_string (input_lines chan))
