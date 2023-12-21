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
  | p0 :: p1 :: ps ->
      (* TODO: adjust for excessive area. Compute area in 1/100's,
         take into account 1/2 (on sides), 1/4 and 3/4 (external and
         internal angles). *)
      let rec do_area total i = function
        | [] -> total
        | pi :: pj :: ps ->
            check_aligned pi pj;
            let part = 100 * shoelace pi pj in
            Printf.eprintf "p%d (%d, %d) × p%d (%d, %d) = %d\n%!" i (fst pi) (snd pi) (i + 1)
              (fst pj) (snd pj) part;
            do_area (total + part) (i + 1) (pj :: ps)
        | [ pi ] ->
            check_aligned pi p0;
            let part = 100 * shoelace pi p0 in
            Printf.eprintf "p%d (%d, %d) × p%d (%d, %d) = %d\n%!" i (fst pi) (snd pi) 0 (fst p0)
              (snd p0) part;
            total + part
      in
      let total = do_area 0 0 (p0 :: p1 :: ps) in
      Int.abs total / (2 * 100)
  | [ _ ] | [] -> 0
