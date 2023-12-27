module Pos : sig
  type t = int * int
  (** Position in a 2D grid. [(0, 0)] is the top-left corner and
    [(n_rows - 1, n_cols - 1)] is the bottom-right corner. *)

  val add : t -> t -> t
  val mul_int : t -> int -> t
  val sub : t -> t -> t
  val is_valid : int * int -> t -> bool
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

type 'a t = 'a array array

val size : 'a t -> int * int
val get_pos : 'a t -> Pos.t -> 'a
val set_pos : 'a t -> Pos.t -> 'a -> unit
val ( .@() ) : 'a t -> Pos.t -> 'a
val ( .@()<- ) : 'a t -> Pos.t -> 'a -> unit
val of_lines : (char -> 'a) -> string Seq.t -> 'a t

val pp :
  ?highlight:Pos.t list ->
  ?sgr:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a t ->
  unit
