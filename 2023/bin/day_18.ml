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
