open Advent

module Point = struct
  type t = { x : int; y : int; z : int }

  let zero = { x = 0; y = 0; z = 0 }
  let map2 f p q = { x = f p.x q.x; y = f p.y q.y; z = f p.z q.z }
  let add = map2 ( + )
  let sub = map2 ( - )
  let of_tuple (x, y, z) = { x; y; z }
  let to_tuple { x; y; z } = (x, y, z)

  let of_string s =
    match
      String.split_on_char ',' s |> List.filter (( <> ) "") |> List.map (int_of_string % String.trim)
    with
    | [ x; y; z ] -> { x; y; z }
    | _ -> invalid_arg __FUNCTION__

  let pp fmt { x; y; z } = Format.fprintf fmt "(%d, %d, %d)" x y z
end

module Brick = struct
  type t = Point.t * Point.t

  let size (min, max) =
    let Point.{ x; y; z } = Point.sub max min in
    (x + 1, y + 1, z + 1)

  let of_string s =
    match String.split_on_char '~' s with
    | [ min; max ] -> (Point.of_string min, Point.of_string max)
    | _ -> invalid_arg __FUNCTION__

  (** Positions of the bottom face of the brick. *)
  let bottom (Point.{ x = x1; y = y1; _ }, Point.{ x = x2; y = y2; _ }) =
    Seq.(product (ints ~until:x2 x1) (ints ~until:y2 y1)) |> List.of_seq

  let pp fmt (min, max) =
    Format.(
      pp_print_char fmt '(';
      Point.pp fmt min;
      pp_print_string fmt ", ";
      Point.pp fmt max;
      pp_print_char fmt ')')
end

module Snapshot = struct
  type t = Brick.t list

  let of_lines = List.of_seq % Seq.map Brick.of_string

  let grid_span bricks =
    let min = List.reduce (Point.map2 Int.min) (List.map fst bricks) in
    let max = List.reduce (Point.map2 Int.max) (List.map snd bricks) in
    (min, max)

  let grid_size bricks = Brick.size (grid_span bricks)

  let sort bricks =
    List.sort
      (fun (min1, max1) (min2, max2) ->
        Stdlib.compare Point.(min1.z, max1.z) Point.(min2.z, max2.z))
      bricks

  let settle bricks =
    let nx, ny, _ = grid_size bricks in
    let h = Array.make_matrix nx ny 0 in
    let id = Array.make_matrix nx ny None in
    List.mapi
      (fun i ((min, max) as brick) ->
        let bottom = Brick.bottom brick in
        let max_h, _ =
          List.map (fun pos -> (Grid.get_pos h pos, pos)) bottom |> List.reduce Stdlib.max
        in
        List.iter
          (fun pos ->
            Grid.set_pos h pos (max_h + 1);
            Grid.set_pos id pos (Some i))
          bottom;
        let dpos = Point.{ zero with z = min.z - (max_h + 1) } in
        Point.(sub min dpos, sub max dpos))
      bricks

  let pp fmt bricks =
    Format.(
      pp_print_list ~pp_sep:pp_print_newline Brick.pp fmt bricks;
      pp_print_newline err_formatter ())
end
