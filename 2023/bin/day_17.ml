open Advent

module Dir = struct
  type t = Up | Left | Right | Down

  let add_pos dir (row, col) =
    match dir with
    | Up -> (row - 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)
    | Down -> (row + 1, col)
end

module Pos = struct
  type t = int * int

  let add (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)
  let add_dir (row, col) dir = Dir.add_pos dir (row, col)
end

module Grid : sig
  type t

  val min_path : t -> int
  val of_lines : string Seq.t -> t
  val size : t -> int * int
end = struct
  type t = int array array

  let of_lines =
    let digit c = int_of_char c - int_of_char '0' in
    Array.of_seq % Seq.map (Array.of_seq % Seq.map digit % String.to_seq)

  let size = Array.matrix_size

  let min_path grid =
    let n_rows, n_cols = size grid in
    (* let valid (row, col) = row >= 0 && row < n_rows && col >= 0 && col < n_cols in *)
    let cells = Array.make_matrix n_rows n_cols None in
    cells.(0).(0) <- Some (0, (0, 0));
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        List.iter
          (fun off ->
            (* Get other position. *)
            (* Is it inside the grid? *)
            (* Is there some state in other position? *)
            (* Is not (offset = other offset = other other offset = other other other offset)? *)
            (* Is this none or other is better? Then update this distance. *)
            (* TODO *)
            ignore off)
          [ (0, -1); (-1, 0); (1, 0); (0, 1) ]
      done
    done;
    Option.(get (map fst cells.(n_rows - 1).(n_cols - 1)))
end
