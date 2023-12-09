include Stdlib.List

let reduce f = function
  | x0 :: xs -> fold_left f x0 xs
  | [] -> invalid_arg __FUNCTION__
