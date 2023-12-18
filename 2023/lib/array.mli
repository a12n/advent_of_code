include module type of Stdlib.Array

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
val symmetry : ('a -> 'a -> bool) -> 'a t -> int option
val transpose : 'a t t -> 'a t t
