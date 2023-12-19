include module type of Stdlib.Array

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val fold_lefti : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val matrix_size : 'a t t -> int * int
val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
val symmetry : ('a -> 'a -> bool) -> 'a t -> int option
val transpose : 'a t t -> 'a t t
val value : 'a t -> default:'a -> int -> 'a
