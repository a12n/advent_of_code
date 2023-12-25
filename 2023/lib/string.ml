include Stdlib.String

let find_index ?(pos = 0) ?len s pred =
  let n = length s in
  if pos < 0 || pos >= n then invalid_arg __FUNCTION__;
  let len = match len with Some len -> len | None -> n - pos in
  let rec loop pos len =
    if len = 0 then None else if pred (get s pos) then Some pos else loop (pos + 1) (len - 1)
  in
  loop pos len

let rec split_on_chars sep s =
  match sep with
  | [] -> [ s ]
  | c :: cs -> List.(concat (map (split_on_chars cs) (split_on_char c s)))

let to_list s = List.init (length s) (unsafe_get s)
