include module type of Stdlib.Array

val equal : ?dist:int -> ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val fill : ?pos:int -> ?len:int -> 'a t -> 'a -> unit
val fold_left2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
val fold_lefti : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val hamming_dist : ?pos:int -> ?len:int -> ('a -> 'b -> bool) -> 'a t -> 'b t -> int
val is_palindrome : ?dist:int -> ?pos:int -> ?len:int -> ('a -> 'a -> bool) -> 'a t -> bool
val matrix_size : 'a t t -> int * int
val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
val symmetry : ('a -> 'a -> bool) -> 'a t -> int option
val rev : 'a t -> 'a t
val rev_inplace : 'a t -> unit

val rotate : [ `CW | `CCW ] -> 'a t t -> 'a t t
(** Reoreder matrix elements, rotate 90° CW/CWW. *)

val transpose : 'a t t -> 'a t t
val value : 'a t -> default:'a -> int -> 'a
val get_mod : 'a t -> int -> 'a
val set_mod : 'a t -> int -> 'a -> unit
