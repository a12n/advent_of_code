(** Gauss's area formula adjusted for integer 2D grids. Polygon is a
    list of points on a grid (the input) connected with implicit lines. *)

let area =
  let check_aligned p0 p1 =
    (* All lines in a polygon must be axis-aligned. *)
    if not (Pos.is_aligned p0 p1) then invalid_arg __FUNCTION__
  in
  let shoelace (ri, ci) (rj, cj) =
    (* Triangle formula, for a point and the next point. *)
    (ci * rj) - (cj * ri)
  in
  function
  | (r0, c0) :: p1 :: ps ->
      (* TODO: adjust for excessive area. Compute area in 1/100's,
         take into account 1/2 (on sides), 1/4 and 3/4 (external and
         internal angles). *)
      let rec do_area total = function
        | [] -> total
        | pi :: pj :: ps ->
            check_aligned pi pj;
            do_area (total + (100 * shoelace pi pj)) (pj :: ps)
        | [ pn ] -> do_area total [ pn; (r0, c0) ]
      in
      let total = do_area 0 ((r0, c0) :: p1 :: ps) in
      Int.abs total / (2 * 100)
  | [ _ ] | [] -> 0
