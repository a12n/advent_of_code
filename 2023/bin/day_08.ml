open Advent

module Dir = struct
  type t = Left | Right

  let of_char = function
    | 'L' | 'l' -> Left
    | 'R' | 'r' -> Right
    | _ -> invalid_arg __FUNCTION__
end

module ID : sig
  type t = private char * char * char

  val compare : t -> t -> int
  val min_value : t
  val max_value : t
  val is_dest : t -> bool
  val is_source : t -> bool
  val of_string : string -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val to_int : t -> int
  val to_string : t -> string
end = struct
  type t = char * char * char

  let compare = Stdlib.compare
  let min_value, max_value = (('A', 'A', 'A'), ('Z', 'Z', 'Z'))
  let is_dest (_, _, c2) = c2 = 'Z'
  let is_source (_, _, c2) = c2 = 'A'

  let to_int =
    let a = int_of_char 'A' in
    let n = int_of_char 'Z' - a + 1 in
    fun (c0, c1, c2) ->
      ((int_of_char c0 - a) * n * n)
      + ((int_of_char c1 - a) * n)
      + (int_of_char c2 - a)

  let of_string s =
    let open String in
    if length s <> 3 then invalid_arg __FUNCTION__;
    (unsafe_get s 0, unsafe_get s 1, unsafe_get s 2)

  let to_string (c0, c1, c2) =
    let open Bytes in
    let b = create 3 in
    unsafe_set b 0 c0;
    unsafe_set b 1 c1;
    unsafe_set b 2 c2;
    unsafe_to_string b

  let pp fmt (c0, c1, c2) =
    Format.(
      pp_print_char fmt c0;
      pp_print_char fmt c1;
      pp_print_char fmt c2)
end

module Network : sig
  type t

  val add : t -> ID.t -> ID.t * ID.t -> t
  val make : unit -> t
  val next : t -> Dir.t -> ID.t -> ID.t
  val next_opt : t -> Dir.t -> ID.t -> ID.t option
end = struct
  type t = (ID.t, ID.t * ID.t) Hashtbl.t

  let make () = Hashtbl.create 1024

  let add tbl src dest =
    Hashtbl.add tbl src dest;
    tbl

  let next_opt tbl dir src =
    match (Hashtbl.find_opt tbl src, dir) with
    | Some (left, _), Dir.Left -> Some left
    | Some (_, right), Dir.Right -> Some right
    | None, _ -> None

  let next tbl dir src = Option.get (next_opt tbl dir src)
end

let input chan =
  let dirs =
    let s = input_line chan in
    Array.init (String.length s) (fun i -> Dir.of_char s.[i])
  in
  if input_line chan <> "" then invalid_arg __FUNCTION__;
  ( dirs,
    Advent.input_lines chan
    |> Seq.map (fun line ->
           Scanf.sscanf line " %3s = ( %3s , %3s ) " (fun from left right ->
               (ID.of_string from, (ID.of_string left, ID.of_string right)))) )
