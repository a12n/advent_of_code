include Stdlib.Seq

let reduce f s =
  match uncons s with
  | Some (x0, xs) -> fold_left f x0 xs
  | None -> invalid_arg __FUNCTION__
