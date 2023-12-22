open Advent

module Dig = struct
  type t = { dir : Dir.t; length : int; color : int }

  let of_string s =
    Scanf.sscanf s " %c %d (#%06x) " (fun dir length color ->
        {
          dir =
            (match dir with
            | 'U' -> Dir.Up
            | 'L' -> Dir.Left
            | 'R' -> Dir.Right
            | 'D' -> Dir.Down
            | _ -> invalid_arg __FUNCTION__);
          length = (if length > 0 then length else invalid_arg __FUNCTION__);
          color;
        })

  let decode { color; _ } =
    {
      dir =
        (match color land 0xF with
        | 0 -> Dir.Right
        | 1 -> Dir.Down
        | 2 -> Dir.Left
        | 3 -> Dir.Up
        | _ -> invalid_arg __FUNCTION__);
      length =
        (match (color lsr 4) land 0xFFFFF with
        | length when length > 0 -> length
        | _ -> invalid_arg __FUNCTION__);
      color = 0;
    }
end

module Plan = struct
  type t = Dig.t list

  let of_lines = List.of_seq % Seq.map Dig.of_string
  let of_lines2 = List.of_seq % Seq.map Dig.(decode % of_string)

  let to_list_pos plan =
    let _, list =
      List.fold_left_map
        (fun pos Dig.{ dir; length; _ } ->
          let pos = Pos.(add pos (mul_int Dir.(to_pos dir) length)) in
          (pos, pos))
        (0, 0) plan
    in
    list

  let min_max_pos plan =
    let _, min, max =
      List.fold_left
        (fun (((row, col) as pos), (min_row, min_col), (max_row, max_col)) Dig.{ dir; length; _ } ->
          let v = Dir.to_pos dir in
          let pos = Pos.(add pos (mul_int v length)) in
          (pos, Int.(min min_row row, min min_col col), Int.(max max_row row, max max_col col)))
        ((0, 0), (max_int, max_int), (min_int, min_int))
        plan
    in
    (min, max)
end

module Grid = struct
  type t = [ `Ground | `Trench ] option array array

  let of_plan plan =
    let (min_row, min_col), (max_row, max_col) = Plan.min_max_pos plan in
    Printf.eprintf "(%d, %d) ~ (%d, %d)\n%!" min_row min_col max_row max_col;
    let n_rows, n_cols = (max_row - min_row + 1, max_col - min_col + 1) in
    Printf.eprintf "n_rows %d, n_cols %d\n%!" n_rows n_cols;
    let grid = Array.make_matrix n_rows n_cols None in
    let rec do_dig ((row, col) as pos) = function
      | [] -> ()
      | Dig.{ length = 0; _ } :: plan -> do_dig pos plan
      | (Dig.{ dir; length; _ } as dig) :: plan ->
          let row', col' = Pos.sub (row, col) (min_row, min_col) in
          grid.(row').(col') <- Some `Trench;
          do_dig (Pos.add pos (Dir.to_pos dir)) (Dig.{ dig with length = length - 1 } :: plan)
    in
    do_dig (0, 0) plan;
    grid

  let dig_interior grid =
    let ((n_rows, n_cols) as size) = Array.matrix_size grid in
    let queue = Queue.create () in
    (* Start from grid borders. *)
    for col = 0 to n_cols - 1 do
      Queue.add (0, col) queue;
      Queue.add (n_rows - 1, col) queue
    done;
    for row = 1 to n_rows - 2 do
      Queue.add (row, 0) queue;
      Queue.add (row, n_cols - 1) queue
    done;
    (* Fill ground from any undecied position. *)
    while not (Queue.is_empty queue) do
      let ((row, col) as pos) = Queue.take queue in
      match grid.(row).(col) with
      | None ->
          (* Undecied position, mark as ground *)
          grid.(row).(col) <- Some `Ground;
          (* Add valid neighbours to the queue. *)
          Dir.[ Up; Left; Right; Down ]
          |> List.map (Pos.add pos % Dir.to_pos)
          |> List.filter (Pos.is_valid size)
          |> List.iter ((Fun.flip Queue.add) queue)
      | Some _ ->
          (* Already marked as either ground or trench. *)
          ()
    done;
    (* Mark all non-ground as trenches. *)
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        if grid.(row).(col) = None then grid.(row).(col) <- Some `Trench
      done
    done

  let trench_volume grid =
    Array.fold_left
      (Array.fold_left (fun total elt ->
           match elt with
           | Some `Trench -> total + 1
           | Some `Ground -> total
           | None ->
               Printf.eprintf "%s: undecied point" __FUNCTION__;
               total))
      0 grid

  let pp fmt =
    Array.iter (fun line ->
        Array.iter
          (fun elt ->
            Format.pp_print_char fmt
              (match elt with None -> '?' | Some `Ground -> '.' | Some `Trench -> '#'))
          line;
        Format.pp_print_newline fmt ())
  [@@ocaml.toplevel_printer]
end
