include Stdlib.Array

let equal eq a b = length a = length b && for_all2 eq a b

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

(* FIXME: Complicated. *)
let symmetry eq a =
  let n = length a in
  let is_sym pos len =
    try
      for i = 0 to (len / 2) - 1 do
        if not (eq (get a (pos + i)) (get a (pos + len - 1 - i))) then raise Exit
      done;
      true
    with Exit -> false
  in
  let rec loop = function
    (* Skip [m] last/first items from the array. Are remaining
       elements symmetrical around the middle? *)
    | `No_Last, m when m >= n ->
        (* Skipped all the elements, no line of symmetry found. *)
        None
    | `No_Last, m when is_sym 0 (n - m) ->
        (* Skipped last [m] elements and elements from 0 up to (n - m)
           are symmetrical. *)
        Some ((n - m) / 2)
    | `No_Last, m ->
        (* Skipped last [m], no symmetry. Try to skip from the
           beginning. *)
        loop (`No_First, m)
    | `No_First, m when is_sym m (n - m) ->
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
