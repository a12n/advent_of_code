include Stdlib.String

let rec split_on_chars sep s =
  match sep with
  | [] -> [ s ]
  | c :: cs -> List.(concat (map (split_on_chars cs) (split_on_char c s)))

let to_list s = List.init (length s) (unsafe_get s)
