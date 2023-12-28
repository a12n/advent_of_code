type t = private { min : int; max : int }

val empty : t
val of_endpoints : int -> int -> t
val of_length : int -> int -> t
val length : t -> int
val is_disjoint : t -> t -> bool
val is_empty : t -> bool
val inter : t -> t -> t
val union : t -> t -> t
val inter_opt : t -> t -> t option
val union_opt : t -> t -> t option
val diff : t -> t -> t list

val partition : int -> t -> t * t
(** Partitions segment [a, b] to segments [a, k - 1] and [k, b]. *)
