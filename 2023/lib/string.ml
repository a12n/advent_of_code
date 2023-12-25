include Stdlib.String

let find_index_from s i pred =
  let n = length s in
  let rec loop i = if i = n then raise Not_found else if pred (get s i) then i else loop (i + 1) in
  loop i

let rfind_index_from s i pred =
  let rec loop i = if i = -1 then raise Not_found else if pred (get s i) then i else loop (i - 1) in
  loop i

let find_index s = find_index_from s 0
let rfind_index s = rfind_index_from s (length s - 1)

let rec split_on_chars sep s =
  match sep with
  | [] -> [ s ]
  | c :: cs -> List.(concat (map (split_on_chars cs) (split_on_char c s)))

let to_list s = List.init (length s) (unsafe_get s)
