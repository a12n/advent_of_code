include module type of Stdlib.Seq

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
