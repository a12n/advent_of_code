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
      (* 1. Iterate over pairs of points, compute are with shoelace.
         2. Iterate over pairs of lines (triples of points), adjust area (exterior and inside angles). *)
      let rec do_area total i = function
        | pi :: pj :: ps ->
            check_aligned pi pj;
            let part = shoelace pi pj in
            Printf.eprintf "p%d (%d, %d) × p%d (%d, %d) = %d\n%!" i (fst pi) (snd pi) (i + 1)
              (fst pj) (snd pj) part;
            do_area (total + part) (i + 1) (pj :: ps)
        | [ pi ] ->
            check_aligned pi p0;
            let part = shoelace pi p0 in
            Printf.eprintf "p%d (%d, %d) × p%d (%d, %d) = %d\n%!" i (fst pi) (snd pi) 0 (fst p0)
              (snd p0) part;
            total + part
        | [] -> assert false
      in
      let total = do_area 0 0 (p0 :: p1 :: ps) in
      Int.abs total / 2
  | [ _ ] | [] -> 0

let straight_joint (r0, c0) (r1, c1) (r2, c2) = (r0 = r1 && r1 = r2) || (c0 = c1 && c1 = c2)

let right_turn_joint (r0, c0) (r1, c1) (r2, c2) =
  (r0 = r1 && c1 = c2 && r1 < r2) || (c0 = c1 && r1 = r2 && c1 < c2)

let left_turn_joint (r0, c0) (r1, c1) (r2, c2) =
  (r0 = r1 && c1 = c2 && r1 > r2) || (c0 = c1 && r1 = r2 && c1 > c2)

let excessive_area =
  let print_joint i pi j pj k pk =
    Printf.eprintf "p%d (%d, %d) - p%d (%d, %d) - p%d (%d, %d) = %c%c%c\n%!" i (fst pi) (snd pi) j
      (fst pj) (snd pj) k (fst pk) (snd pk)
      (if left_turn_joint pi pj pk then '<' else '.')
      (if straight_joint pi pj pk then '|' else '.')
      (if right_turn_joint pi pj pk then '>' else '.')
  in
  function
  | p0 :: p1 :: p2 :: ps ->
      let rec do_area (total1, total2) i = function
        | pi :: pj :: pk :: ps ->
            (* pi pj pk *)
            print_joint i pi (i + 1) pj (i + 2) pk;
            do_area (total1, total2) (i + 1) (pj :: pk :: ps)
        | [ pi; pj ] ->
            (* pi pj p0 *)
            print_joint i pi (i + 1) pj 0 p0;
            do_area (total1,total2) (i + 1) [ pj ]
        | [ pi ] ->
            (* pi p0 p1 *)
            print_joint i pi 0 p0 1 p1;
            (total1, total2)
        | [] -> assert false
      in
      do_area (0,0) 0 (p0 :: p1 :: p2 :: ps)
  | _ -> (0,0)

let test1 =
  (* 4 *)
  [ (1, 1); (4, 1); (4, 4); (1, 4) ]

let test2 =
  (* 8 *)
  [ (1, 1); (1, 6); (6, 6); (6, 4); (3, 4); (3, 1) ]

let test3 =
  (* 13 *)
  [ (1, 1); (1, 2); (4, 2); (4, 5); (1, 5); (1, 7); (7, 7); (7, 1) ]
