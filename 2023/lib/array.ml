include Stdlib.Array

let equal eq a b = length a = length b && for_all2 eq a b

let reduce f a =
  let n = length a in
  if n < 1 then invalid_arg __FUNCTION__;
  let r = ref (unsafe_get a 0) in
  for i = 1 to n - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let symmetric eq a pos len =
  Int.is_even len
  &&
  try
    for i = 0 to (len / 2) - 1 do
      let j = len - 1 - i in
      if not (eq (get a (pos + i)) (get a (pos + j))) then raise Exit
    done;
    true
  with Exit -> false

let symmetry _eq _a =
  (* TODO *)
  None

let transpose a =
  let n_rows = length a in
  let n_cols = if n_rows > 0 then length (unsafe_get a 0) else 0 in
  init n_cols (fun col -> init n_rows (fun row -> get (get a row) col))
