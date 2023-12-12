include module type of Stdlib.List

val diffs : ('a -> 'a -> 'b) -> 'a list -> 'b list

val pairs : 'a list -> ('a * 'a) list
(** Turn [[x0; x1; x2; x3; …]] to [[(x0, x1); (x2, x3); …]]. *)

val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

val unpair : ('a * 'a) list -> 'a list
(** Inverse of [pairs]. *)
