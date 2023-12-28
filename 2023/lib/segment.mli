type t = private { min : int; max : int }

val make : int -> [ `End of int | `Length of int ] -> t
val length : t -> int
val inter : t -> t -> t
val union : t -> t -> t
val inter_opt : t -> t -> t option
val union_opt : t -> t -> t option
val diff : t -> t -> t list
