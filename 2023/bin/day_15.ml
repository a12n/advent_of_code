open Advent

let update_hash h c = 17 * (h + int_of_char c) mod 256
let hash = String.fold_left update_hash 0

module Step = struct
  type t = { label : string; op : [ `Insert of int | `Remove ] }

  let of_string s =
    let digit c =
      if c >= '0' && c <= '9' then int_of_char c - int_of_char '0' else invalid_arg __FUNCTION__
    in
    let n = String.length s in
    if n < 2 then invalid_arg __FUNCTION__;
    if s.[n - 1] = '-' then { label = String.sub s 0 (n - 1); op = `Remove }
    else if s.[n - 2] = '=' then { label = String.sub s 0 (n - 2); op = `Insert (digit s.[n - 1]) }
    else invalid_arg __FUNCTION__
end

module Lens_System : sig
  type t = private (string * int) list
  (** Lens in the reverse order. *)

  val empty : t
  val insert : t -> string -> int -> t
  val remove : t -> string -> t
end = struct
  type t = (string * int) list

  let empty = []
  let insert lens label fd = List.replace_assoc label fd lens
  let remove lens label = List.remove_assoc label lens
end

module Boxes : sig
  type t

  val focus_power : t -> int
  val make : unit -> t
  val perform : t -> Step.t -> unit
end = struct
  type t = Lens_System.t array

  let focus_power =
    Array.fold_lefti
      (fun total i lens ->
        List.rev (lens : Lens_System.t :> (string * int) list)
        |> List.mapi (fun j (_, fd) -> (i + 1) * (j + 1) * fd)
        |> List.fold_left ( + ) total)
      0

  let make () = Array.make 256 Lens_System.empty

  let perform boxes = function
    | Step.{ label; op = `Remove } ->
        let i = hash label in
        boxes.(i) <- Lens_System.remove boxes.(i) label
    | Step.{ label; op = `Insert fd } ->
        let i = hash label in
        boxes.(i) <- Lens_System.insert boxes.(i) label fd
end
