include module type of Stdlib.List

val diffs : ('a -> 'a -> 'b) -> 'a list -> 'b list
val intersperse : 'a -> 'a list -> 'a list
val last : 'a list -> 'a
val make : int -> 'a -> 'a list
val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

val combine_tl : 'a list -> ('a * 'a list) list
(** Combine each element [a] with the tail after [a]. To process all unordered pairs of elements in the list similar to:
    [
	for i = 1 to n do
		for j = i + 1 to n do
			process_pair elts.(i) elts.(j)
		done
	done
    ]
*)

val take_pairs : 'a list -> ('a * 'a) list
(** Turn [[x0; x1; x2; x3; …]] to [[(x0, x1); (x2, x3); …]]. *)

val untake_pairs : ('a * 'a) list -> 'a list
(** Inverse of [pairs]. *)
