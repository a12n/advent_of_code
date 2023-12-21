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
end

module Plan = struct
  type t = Dig.t list

  let of_lines = List.of_seq % Seq.map Dig.of_string

  let size plan =
    let _, (max_row, max_col) =
      List.fold_left
        (fun (pos, max) Dig.{ dir; length; _ } ->
          let v = Dir.to_pos dir in
          let pos = Pos.(add pos (mul_int v length)) in
          (pos, Stdlib.max max pos))
        ((0, 0), (0, 0))
        plan
    in
    (max_row + 1, max_col + 1)
end

module Grid = struct
  type t = [ `Ground | `Trench ] option array array

  let of_plan plan =
    let n_rows, n_cols = Plan.size plan in
    let grid = Array.make_matrix n_rows n_cols None in
    let rec do_dig ((row, col) as pos) = function
      | [] -> ()
      | Dig.{ length = 0; _ } :: plan -> do_dig pos plan
      | (Dig.{ dir; length; _ } as dig) :: plan ->
          grid.(row).(col) <- Some `Trench;
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
      (fun total line ->
        Array.fold_left
          (fun total elt ->
            match elt with
            | Some `Trench -> total + 1
            | Some `Ground -> total
            | None ->
                Printf.eprintf "%s: undecied point" __FUNCTION__;
                total)
          total line)
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
