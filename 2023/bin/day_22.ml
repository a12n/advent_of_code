open Advent
open Hashtbl.Ops
module Int_Set = Set.Make (Int)
module Point = Point.Integer

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

  let of_lines =
    List.sort (fun (min1, max1) (min2, max2) ->
        Stdlib.compare Point.(min1.z, max1.z) Point.(min2.z, max2.z))
    % List.of_seq % Seq.map Brick.of_string

  let grid_span bricks =
    let min = List.reduce (Point.map2 Int.min) (List.map fst bricks) in
    let max = List.reduce (Point.map2 Int.max) (List.map snd bricks) in
    (min, max)

  let grid_size bricks = Brick.size (grid_span bricks)

  (** Must be called on bricks sorted by [z]. *)
  let settle bricks =
    let height = Hashtbl.create 256 in
    let id = Hashtbl.create 256 in
    let supports, supported = Hashtbl.(create 1024, create 1024) in
    ( List.mapi (fun i ((min, max) as brick) ->
          let _, _, size_z = Brick.size brick in
          let bottom = Brick.bottom brick in
          let sup_z =
            List.map (Option.value ~default:0 % Hashtbl.find_opt height) bottom
            |> List.reduce Int.max
          in
          List.filter_map
            (fun pos ->
              let lies_on = if height.%%{pos} = Some sup_z then id.%%{pos} else None in
              id.%{pos} <- i;
              height.%{pos} <- sup_z + size_z;
              lies_on)
            bottom
          |> List.sort_uniq Int.compare
          |> List.iter (fun j ->
                 Hashtbl.add supports i j;
                 Hashtbl.add supported j i);
          let drop = Point.{ zero with z = min.z - (sup_z + 1) } in
          Point.(sub min drop, sub max drop)) bricks,
      supports,
      supported )

  let pp fmt bricks =
    Format.(
      pp_print_list ~pp_sep:pp_print_newline Brick.pp fmt bricks;
      pp_print_newline err_formatter ())
end
