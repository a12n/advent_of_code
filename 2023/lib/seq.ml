include Stdlib.Seq

let reduce f s =
  match uncons s with Some (x0, xs) -> fold_left f x0 xs | None -> invalid_arg __FUNCTION__

module Symmetric = struct
  let ints pos len =
    if len < 0 || pos < 0 then invalid_arg __FUNCTION__;
    let mid = len / 2 in
    unfold (fun i -> if i < mid then Some ((pos + i, pos + len - 1 - i), i + 1) else None) 0

  (* FIXME: Complicated? *)
  let windows n =
    if n < 0 then invalid_arg __FUNCTION__;
    (* Skip [m] last/first items from the array. Are remaining elements
       symmetrical around the middle? *)
    unfold
      (function
        | `No_Last, m when m >= n ->
            (* Skipped all the elements. *)
            None
        | `No_Last, m ->
            (* Skip last [m] elements, elements from 0 up to (n - m) is the part for symmetry check. *)
            let part = (0, n - m) in
            Some (part, (`No_First, m))
        | `No_First, m ->
            (* Skip first [m] elements, emit part. Then skip from the end again, but this time skip more elements. *)
            let part = (m, n - m) in
            Some (part, (`No_Last, m + 2)))
      (`No_Last, if Int.is_even n then 0 else 1)
end
