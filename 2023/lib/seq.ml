include Stdlib.Seq

let reduce f s =
  match uncons s with Some (x0, xs) -> fold_left f x0 xs | None -> invalid_arg __FUNCTION__

let symmetric pos len =
  if len < 0 then invalid_arg __FUNCTION__;
  if pos < 0 || pos >= len then invalid_arg __FUNCTION__;
  let mid = len / 2 in
  unfold (fun i -> if i < mid then Some ((pos + i, pos + len - 1 - i), i + 1) else None) 0
