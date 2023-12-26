include Stdlib.Array

let fold_left2 f acc a b =
  let n = length a in
  if length b <> n then invalid_arg __FUNCTION__;
  let r = ref acc in
  for i = 0 to n - 1 do
    r := f !r (unsafe_get a i) (unsafe_get b i)
  done;
  !r

let equal ?(dist = 0) eq a b =
  if dist = 0 then length a = length b && for_all2 eq a b
  else if dist > 0 then
    fold_left2 (fun dist ai bi -> if eq ai bi then dist else dist - 1) dist a b >= 0
  else invalid_arg __FUNCTION__

let fold_lefti f acc a =
  let r = ref acc in
  for i = 0 to length a - 1 do
    r := f !r i (unsafe_get a i)
  done;
  !r

let matrix_size a =
  let n_rows = length a in
  let n_cols = if n_rows > 0 then length (unsafe_get a 0) else 0 in
  (n_rows, n_cols)

let reduce f a =
  let n = length a in
  if n < 1 then invalid_arg __FUNCTION__;
  let r = ref (unsafe_get a 0) in
  for i = 1 to n - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let is_palindrome ?(dist = 0) ?(pos = 0) ?len eq a =
  let n = length a in
  let len = match len with None -> n - pos | Some len -> len in
  if (pos < 0 || pos >= n) || len < 0 || pos + len > n then invalid_arg __FUNCTION__;
  let mid = len / 2 in
  let rec loop = function
    | dist, _ when dist < 0 -> false
    | dist, i when i < mid ->
        let ai = get a (pos + i) in
        let aj = get a (pos + len - 1 - i) in
        loop ((if eq ai aj then dist else dist - 1), i + 1)
    | _, _ -> true
  in
  loop (dist, 0)

(* FIXME: Complicated. *)
let symmetry eq a =
  let n = length a in
  let rec loop = function
    (* Skip [m] last/first items from the array. Are remaining
       elements symmetrical around the middle? *)
    | `No_Last, m when m >= n ->
        (* Skipped all the elements, no line of symmetry found. *)
        None
    | `No_Last, m when is_palindrome ~pos:0 ~len:(n - m) eq a ->
        (* Skipped last [m] elements and elements from 0 up to (n - m)
           are symmetrical. *)
        Some ((n - m) / 2)
    | `No_Last, m ->
        (* Skipped last [m], no symmetry. Try to skip from the
           beginning. *)
        loop (`No_First, m)
    | `No_First, m when is_palindrome ~pos:m ~len:(n - m) eq a ->
        (* No first [m] elements and elements from m up to n are
           symmetrical. *)
        Some (m + ((n - m) / 2))
    | `No_First, m ->
        (* Skipped first [m] elements, no symmetry. Skip from the end
           again, but this time skip more elements. *)
        loop (`No_Last, m + 2)
  in
  if Int.is_even n then loop (`No_Last, 0) else loop (`No_Last, 1)

let transpose a =
  let n_rows, n_cols = matrix_size a in
  init n_cols (fun col -> init n_rows (fun row -> get (get a row) col))

let value a ~default i = if i >= 0 && i < length a then unsafe_get a i else default
