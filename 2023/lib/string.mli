include module type of Stdlib.String

val find_index : string -> (char -> bool) -> int
val find_index_from : string -> int -> (char -> bool) -> int
val rfind_index : string -> (char -> bool) -> int
val rfind_index_from : string -> int -> (char -> bool) -> int
val split_on_chars : char list -> string -> string list
val to_list : string -> char list
