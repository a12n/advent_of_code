include module type of Stdlib.String

val find_index : ?pos:int -> ?len:int -> string -> (char -> bool) -> int option
val split_on_chars : char list -> string -> string list
val to_list : string -> char list
