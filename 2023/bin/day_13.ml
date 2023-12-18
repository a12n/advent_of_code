open Advent

module Note = struct
  type t = Ash | Rock

  let of_char = function '.' -> Ash | '#' -> Rock | _ -> invalid_arg __FUNCTION__
end

module Grid = struct
  type t = Note.t array array

  let size notes =
    let rows = Array.length notes in
    let cols = if rows > 0 then Array.(length (unsafe_get notes 0)) else 0 in
    (rows, cols)

  let of_lines =
    Array.of_seq
    % Seq.map (Array.of_seq % Seq.map Note.of_char % String.to_seq)
    % Seq.take_while (( <> ) "")
end

let input chan =
  Seq.unfold
    (fun lines ->
      match Seq.uncons lines with
      | Some (line, lines') -> Some (Grid.of_lines (Seq.cons line lines'), lines)
      | None -> None)
    (input_lines chan)
