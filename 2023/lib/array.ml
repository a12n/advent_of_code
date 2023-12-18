include Stdlib.Array

let reduce f a =
  let n = length a in
  if n < 1 then invalid_arg __FUNCTION__;
  let r = ref (unsafe_get a 0) in
  for i = 1 to n - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let transpose a =
  let n_rows = Array.length a in
  let n_cols = if n_rows > 0 then Array.(length (unsafe_get a 0)) else 0 in
  Array.init n_cols (fun col -> Array.init n_rows (fun row -> a.(row).(col)))
