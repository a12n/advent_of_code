module Line = struct
  type t = (int * int) * (int * int)

  let pp ?(i = 0) fmt (p1, p2) =
    Format.fprintf fmt "p%d " i;
    Pos.pp fmt p1;
    Format.fprintf fmt " - p%d " (i + 1);
    Pos.pp fmt p2

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
  type t = (int * int) * (int * int) * (int * int)

  let pp ?(i = 0) fmt (p1, p2, p3) =
    Format.fprintf fmt "p%d " i;
    Pos.pp fmt p1;
    Format.fprintf fmt " - p%d " (i + 1);
    Pos.pp fmt p2;
    Format.fprintf fmt " - p%d " (i + 2);
    Pos.pp fmt p3

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

  module CCW = struct
    (* TODO: Looks like day 18 is CW. *)
    let is_internal_angle ((x1, y1), (x2, y2), (x3, y3)) =
      (x1 = x2 && y1 < y2 && x2 > x3 && y2 = y3)
      || (x1 > x2 && y1 = y2 && x2 = x3 && y2 > y3)
      || (x1 = x2 && y1 > y2 && x2 < x3 && y2 = y3)
      || (x1 < x2 && y1 = y2 && x2 = x3 && y2 < y3)

    (* TODO: Looks like day 18 is CW. *)
    let is_external_angle ((x1, y1), (x2, y2), (x3, y3)) =
      (x1 = x2 && y1 < y2 && x2 < x3 && y2 = y3)
      || (x1 > x2 && y1 = y2 && x2 = x3 && y2 < y3)
      || (x1 = x2 && y1 > y2 && x2 > x3 && y2 = y3)
      || (x1 < x2 && y1 = y2 && x2 = x3 && y2 > y3)
  end

  let part_area j =
    (* TODO: is_forward, is_backward *)
    if CCW.is_internal_angle j then 25
    else if CCW.is_external_angle j then 75
    else invalid_arg __FUNCTION__
end

let compact = function
  | [] -> []
  | [ _ ] as points -> points
  | [ _; _ ] as points -> points
  | p0 :: p1 :: ps ->
      let rec loop = function
        | pi :: pj :: pk :: ps when Joint.is_forward (pi, pj, pk) -> loop (pi :: pk :: ps)
        | [ pi; pj ] when Joint.is_forward (pi, pj, p0) -> [ pi ]
        | pi :: ps -> pi :: loop ps
        | [] -> []
      in
      if Joint.is_forward (List.last ps, p0, p1) then loop (p1 :: ps) else loop (p0 :: p1 :: ps)

let boundary_area points =
  let lines = Line.list_of_polygon points in
  if not (List.for_all Line.is_aligned lines) then invalid_arg __FUNCTION__;
  List.fold_left (fun area line -> area + (2 * Line.part_area line / 100) + 1) 0 lines

(** Gauss's area formula adjusted for integer 2D grids. Polygon is a
    list of points on a grid (the input) connected with implicit lines. *)
let interior_area points =
  let lines = Line.list_of_polygon points in
  if not (List.for_all Line.is_aligned lines) then
    invalid_arg (__FUNCTION__ ^ ": all lines must be axis-aligned");
  let area = Int.abs List.(fold_left ( + ) 0 (map Line.shoelace lines)) / 2 in
  let joints_area = Joint.list_of_polygon points |> List.map Joint.part_area |> List.reduce ( + ) in
  let lines_area = lines |> List.map Line.part_area |> List.reduce ( + ) in
  (* FIXME: invalid joints_area, invalid is_internal_angle/is_external_angle. *)
  Printf.eprintf "area %d, joints_area %d, lines_area %d\n%!" (100 * area) joints_area lines_area;
  ((100 * area) - joints_area - lines_area) / 100
