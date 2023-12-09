include Stdlib.Array

let reduce f a =
  let n = length a in
  if n < 1 then invalid_arg __FUNCTION__;
  let r = ref (unsafe_get a 0) in
  for i = 1 to n - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r
