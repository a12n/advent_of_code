open Advent

module Note = struct
  type t = Ash | Rock

  let of_char = function '.' -> Ash | '#' -> Rock | _ -> invalid_arg __FUNCTION__
end

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val reflection : t -> [ `Horiz of int | `Vert of int ] option
end = struct
  type t = Note.t array array

  let size notes =
    let rows = Array.length notes in
    let cols = if rows > 0 then Array.(length (unsafe_get notes 0)) else 0 in
    (rows, cols)

  let equal n get i j =
    try
      for k = 0 to n - 1 do
        if get i k <> get j k then raise Exit
      done;
      true
    with Exit -> false

  let rows_equal notes =
    let _, n_cols = size notes in
    equal n_cols (fun row col -> notes.(row).(col))

  let cols_equal notes =
    let n_rows, _ = size notes in
    equal n_rows (fun col row -> notes.(row).(col))

  let of_lines =
    Array.of_seq
    % Seq.map (Array.of_seq % Seq.map Note.of_char % String.to_seq)
    % Seq.take_while (( <> ) "")

  let reflection _notes =
    (* TODO *)
    None
end

let input chan =
  Seq.unfold
    (fun lines ->
      match Seq.uncons lines with
      | Some (line, lines') -> Some (Grid.of_lines (Seq.cons line lines'), lines)
      | None -> None)
    (input_lines chan)
