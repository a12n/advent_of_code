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
  val distances : t -> int * int -> (int * (int * int)) array array
end = struct
  type t = int array array

  let get a (row, col) = a.(row).(col)
  let set a (row, col) value = a.(row).(col) <- value

  let distances grid start =
    let ( let* ) = Option.bind in
    let n_rows, n_cols = Array.matrix_size grid in
    let max_row, max_col = (n_rows - 1, n_cols - 1) in
    let validate = function
      | row, col when row < 0 || col < 0 -> None
      | row, col when row > max_row || col > max_col -> None
      | row, col -> Some (row, col)
    in
    let cells = Array.make_matrix n_rows n_cols (max_int, (0, 0)) in
    let queue = Queue.create () in
    set cells start (0, (0,0));
    Queue.add start queue;
    while not (Queue.is_empty queue) do
      let pos = Queue.take queue in
      [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
      |> List.filter_map (fun off ->
             let* pos' = validate (Pos.add pos off) in
             Some (pos', off))
      |> List.filter_map (fun (pos', off) ->
          Queue.add pos' queue;
          let ((dist, _) as cell') = get cells pos' in
          if dist < max_int then Some (cell', pos', off) else None
        )
      |> List.iter (fun (cell', pos', off) ->
          let (dist', off') = get cells pos' in
          (* TODO *)
          ignore (off, (row', col')))
    done;
    cells

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
