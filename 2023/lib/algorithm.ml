(* FIXME: Complicated? *)
let symmetry_seq n =
  if n < 0 then invalid_arg __FUNCTION__;
  (* Skip [m] last/first items from the array. Are remaining elements
     symmetrical around the middle? *)
  Seq.unfold
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
