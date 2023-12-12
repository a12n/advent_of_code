open Advent

module Dir = struct
  type t = Left | Right

  let of_char = function
    | 'L' | 'l' -> Left
    | 'R' | 'r' -> Right
    | _ -> invalid_arg __FUNCTION__
end

module ID : sig
  type t = private int

  val max_value : t
  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string
  val zero : t
end = struct
  type t = int

  let base, offset = (26, int_of_char 'A')

  let of_string s =
    if String.length s <> 3 then invalid_arg __FUNCTION__;
    String.fold_left
      (fun id c ->
        if c < 'A' || c > 'Z' then invalid_arg __FUNCTION__;
        (id * base) + int_of_char c - offset)
      0 s

  let zero, max_value = (0, of_string "ZZZ")
  let of_int n = if n < 0 || n > max_value then invalid_arg __FUNCTION__ else n

  let to_string id =
    let open Bytes in
    let b = create 3 in
    unsafe_set b 2 (char_of_int ((id mod base) + offset));
    unsafe_set b 1 (char_of_int ((id / base mod base) + offset));
    unsafe_set b 0 (char_of_int ((id / base / base) + offset));
    unsafe_to_string b
end

module Network : sig
  type t

  val make : unit -> t
  val add : t -> ID.t -> ID.t * ID.t -> t
  val left : t -> ID.t -> ID.t
  val right : t -> ID.t -> ID.t
  val next : t -> ID.t -> Dir.t -> ID.t
end = struct
  type t = ID.t array * ID.t array

  let make () =
    let n = (ID.max_value :> int) + 1 in
    (Array.init n ID.of_int, Array.init n ID.of_int)

  let add (l, r) src (left, right) =
    l.((src : ID.t :> int)) <- left;
    r.((src : ID.t :> int)) <- right;
    (l, r)

  let left (l, _) src = l.((src : ID.t :> int))
  let right (_, r) src = r.((src : ID.t :> int))

  let next net src = function
    | Dir.Left -> left net src
    | Dir.Right -> right net src
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
