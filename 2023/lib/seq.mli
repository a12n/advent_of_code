include module type of Stdlib.Seq

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a

module Symmetric : sig
  val ints : int -> int -> (int * int) t
  (** [Symmetric.ints pos len] generates [len / 2] pairs of symmetric
    indicies.  E.g., [Symmetric.ints 10 6] will produce pairs
    [(10, 15); (11, 14); (12, 13)]. *)

  val windows : int -> (int * int) t
  (** [Symmetric.windows n] generates windows of even length into an
      array of length [n]. See day 13 problem. *)
end
