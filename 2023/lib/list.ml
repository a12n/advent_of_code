include Stdlib.List

let rec pairs = function
  | [] -> []
  | x0 :: x1 :: xs -> (x0, x1) :: pairs xs
  | _ -> invalid_arg __FUNCTION__

let reduce f = function
  | x0 :: xs -> fold_left f x0 xs
  | [] -> invalid_arg __FUNCTION__
