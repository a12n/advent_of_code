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

let hamming_dist ?(pos = 0) ?len eq a b =
  let n, m = (length a, length b) in
  let len =
    match len with
    | None ->
        if n <> m then invalid_arg (__FUNCTION__ ^ ": lengths mismatch");
        n - pos
    | Some len -> len
  in
  if pos < 0 || pos >= n || pos >= m then invalid_arg (__FUNCTION__ ^ ": invalid pos");
  if len < 0 || pos + len > n || pos + len > m then invalid_arg (__FUNCTION__ ^ ": invalid len");
  let rec loop dist = function
    | i when i < len ->
        let ai = get a (pos + i) in
        let bi = get b (pos + i) in
        loop (if eq ai bi then dist else dist + 1) (i + 1)
    | _i -> dist
  in
  loop 0 0

let palindrome_hamming_dist ?(pos = 0) ?len eq a =
  let n = length a in
  let len = match len with None -> n - pos | Some len -> len in
  if (pos < 0 || pos >= n) || len < 0 || pos + len > n then invalid_arg __FUNCTION__;
  Seq.fold_left
    (fun dist (i, j) -> if eq (get a i) (get a j) then dist else dist + 1)
    0 (Seq.Symmetric.ints pos len)

let is_palindrome ?(dist = 0) ?pos ?len eq a = palindrome_hamming_dist ?pos ?len eq a <= dist

let symmetry eq a =
  Seq.Symmetric.windows (length a)
  |> Seq.find (fun (pos, len) -> is_palindrome ~pos ~len eq a)
  |> Option.map (fun (pos, len) -> pos + (len / 2))

let transpose a =
  let n_rows, n_cols = matrix_size a in
  init n_cols (fun col -> init n_rows (fun row -> get (get a row) col))

let value a ~default i = if i >= 0 && i < length a then unsafe_get a i else default
