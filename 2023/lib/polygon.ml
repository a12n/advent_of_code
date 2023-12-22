module Line = struct
  let list_of_polygon = function
    | p0 :: p1 :: ps ->
        let rec loop = function
          | pi :: pj :: ps -> (pi, pj) :: loop (pj :: ps)
          | [ pi ] -> [ (pi, p0) ]
          | [] -> failwith __FUNCTION__
        in
        loop (p0 :: p1 :: ps)
    | [ _ ] | [] -> []

  let is_horiz_aligned ((_, y1), (_, y2)) = y1 = y2
  let is_vert_aligned ((x1, _), (x2, _)) = x1 = x2
  let is_aligned l = is_horiz_aligned l || is_vert_aligned l

  let shoelace ((x1, y1), (x2, y2)) =
    (* Trapezoid formula. *)
    (y1 + y2) * (x1 - x2)

  let part_area (((x1, y1), (x2, y2)) as l) =
    let length =
      if is_horiz_aligned l then x2 - x1
      else if is_vert_aligned l then y2 - y1
      else invalid_arg __FUNCTION__
    in
    50 * (Int.abs length - 1)
end

module Joint = struct
  let list_of_polygon = function
    | p0 :: p1 :: p2 :: ps ->
        let rec loop = function
          | pi :: pj :: pk :: ps -> (pi, pj, pk) :: loop (pj :: pk :: ps)
          | [ pi; pj ] -> (pi, pj, p0) :: loop [ pj ]
          | [ pi ] -> [ (pi, p0, p1) ]
          | [] -> failwith __FUNCTION__
        in
        loop (p0 :: p1 :: p2 :: ps)
    | [ _; _ ] | [ _ ] | [] -> []

  let is_horiz_aligned ((_, y1), (_, y2), (_, y3)) = y1 = y2 && y2 = y3
  let is_vert_aligned ((x1, _), (x2, _), (x3, _)) = x1 = x2 && x2 = x3

  let is_horiz_forward (((x1, _), (x2, _), (x3, _)) as j) =
    is_horiz_aligned j && ((x1 < x2 && x2 < x3) || (x1 > x2 && x2 > x3))

  let is_horiz_backward (((x1, _), (x2, _), (x3, _)) as j) =
    is_horiz_aligned j && ((x1 < x2 && x3 < x2) || (x1 > x2 && x3 > x2))

  let is_vert_forward ((x1, y1), (x2, y2), (x3, y3)) =
    is_horiz_forward ((y1, x1), (y2, x2), (y3, x3))

  let is_vert_backward ((x1, y1), (x2, y2), (x3, y3)) =
    is_horiz_backward ((y1, x1), (y2, x2), (y3, x3))

  let is_forward j = is_horiz_forward j || is_vert_forward j
  let is_backward j = is_horiz_backward j || is_vert_backward j

  let is_left_turn (p1, ((x2, y2) as p2), ((x3, y3) as p3)) =
    Line.(is_horiz_aligned (p1, p2) && is_vert_aligned (p2, p3) && y3 > y2)
    || Line.(is_vert_aligned (p1, p2) && is_horiz_aligned (p2, p3) && x3 < x2)

  (* CCW *)
  let is_internal_angle ((x1, y1), (x2, y2), (x3, y3)) =
    (x1 = x2 && y1 < y2 && x2 > x3 && y2 = y3)
    || (x1 > x2 && y1 = y2 && x2 = x3 && y2 > y3)
    || (x1 = x2 && y1 > y2 && x2 < x3 && y2 = y3)
    || (x1 < x2 && y1 = y2 && x2 = x3 && y2 < y3)

  (* CCW *)
  let is_external_angle ((x1, y1), (x2, y2), (x3, y3)) =
    (x1 = x2 && y1 < y2 && x2 < x3 && y2 = y3)
    || (x1 > x2 && y1 = y2 && x2 = x3 && y2 < y3)
    || (x1 = x2 && y1 > y2 && x2 > x3 && y2 = y3)
    || (x1 < x2 && y1 = y2 && x2 = x3 && y2 > y3)

  let part_area j =
    if is_internal_angle j then 25 else if is_external_angle j then 75 else invalid_arg __FUNCTION__
end

(** Gauss's area formula adjusted for integer 2D grids. Polygon is a
    list of points on a grid (the input) connected with implicit lines. *)
let area ps =
  let ls = Line.list_of_polygon ps in
  if not (List.for_all Line.is_aligned ls) then
    invalid_arg (__FUNCTION__ ^ ": all lines must be axis-aligned");
  (* TODO: Compensate for excesive area in lines and joints. *)
  Int.abs List.(fold_left ( + ) 0 (map Line.shoelace ls)) / 2

let straight_joint (r0, c0) (r1, c1) (r2, c2) = (r0 = r1 && r1 = r2) || (c0 = c1 && c1 = c2)

let right_turn_joint (r0, c0) (r1, c1) (r2, c2) =
  (r0 = r1 && c1 = c2 && r1 > r2) || (c0 = c1 && r1 = r2 && c1 < c2)

let left_turn_joint (r0, c0) (r1, c1) (r2, c2) =
  (r0 = r1 && c1 = c2 && r1 < r2) || (c0 = c1 && r1 = r2 && c1 > c2)

(* Excessive area in line joints. *)
let joint_area =
  let print_joint i pi j pj k pk =
    Printf.eprintf "p%d (%d, %d) - p%d (%d, %d) - p%d (%d, %d) = %c%c%c\n%!" i (fst pi) (snd pi) j
      (fst pj) (snd pj) k (fst pk) (snd pk)
      (if left_turn_joint pi pj pk then '<' else '.')
      (if straight_joint pi pj pk then '|' else '.')
      (if right_turn_joint pi pj pk then '>' else '.')
  in
  let part_area pi pj pk =
    if straight_joint pi pj pk then (50, 50)
    else if left_turn_joint pi pj pk then (25, 75)
    else if right_turn_joint pi pj pk then (75, 25)
    else assert false
  in
  function
  | p0 :: p1 :: p2 :: ps ->
      let rec do_area (total1, total2) i = function
        | pi :: pj :: pk :: ps ->
            (* pi pj pk *)
            print_joint i pi (i + 1) pj (i + 2) pk;
            let part1, part2 = part_area pi pj pk in
            do_area (total1 + part1, total2 + part2) (i + 1) (pj :: pk :: ps)
        | [ pi; pj ] ->
            (* pi pj p0 *)
            print_joint i pi (i + 1) pj 0 p0;
            let part1, part2 = part_area pi pj p0 in
            do_area (total1 + part1, total2 + part2) (i + 1) [ pj ]
        | [ pi ] ->
            (* pi p0 p1 *)
            print_joint i pi 0 p0 1 p1;
            let part1, part2 = part_area pi p0 p1 in
            (total1 + part1, total2 + part2)
        | [] -> assert false
      in
      do_area (0, 0) 0 (p0 :: p1 :: p2 :: ps)
  | _ -> (0, 0)

(* Excessive area in straight parts of the lines connecting points. *)
let lines_area =
  function
  | p0 :: ps ->
      let rec do_area total = function
        | pi :: pj :: ps ->
            let part = Line.part_area (pi, pj) in
            do_area (total + part) (pj :: ps)
        | [ pi ] ->
            let part = Line.part_area (pi, p0) in
            total + part
        | [] -> assert false
      in
      do_area 0 (p0 :: ps)
  | [] -> 0

let test_1_1 = [ (1, 1); (4, 1); (4, 4); (1, 4) ]
let test_1_2 = [ (1, 1); (1, 4); (4, 4); (4, 1) ]

let test2 =
  (* 8 *)
  [ (1, 1); (1, 6); (6, 6); (6, 4); (3, 4); (3, 1) ]

let test3 =
  (* 13 *)
  [ (1, 1); (1, 2); (4, 2); (4, 5); (1, 5); (1, 7); (7, 7); (7, 1) ]
