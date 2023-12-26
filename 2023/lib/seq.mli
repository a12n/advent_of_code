include module type of Stdlib.Seq

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a

val symmetric : int -> int -> (int * int) t
(** [symmetric pos len] generates [len / 2] pairs of symmetric
    indicies.  E.g., [symmetric 10 6] will produce pairs
    [(10, 15); (11, 14); (12, 13)]. *)
