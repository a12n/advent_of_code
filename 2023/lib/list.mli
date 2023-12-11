include module type of Stdlib.List

val pairs : 'a list -> ('a * 'a) list
val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
