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

  let of_lines =
    Array.of_seq
    % Seq.map (Array.of_seq % Seq.map Note.of_char % String.to_seq)
    % Seq.take_while (( <> ) "")

  let reflection notes =
    match Array.symmetry (Array.equal ( = )) notes with
    | Some k -> Some (`Horiz k)
    | None -> (
        match Array.symmetry (Array.equal ( = )) (Array.transpose notes) with
        | Some k -> Some (`Vert k)
        | None -> None)
end

let input chan =
  Seq.unfold
    (fun lines ->
      match Seq.uncons lines with
      | Some (line, lines') -> Some (Grid.of_lines (Seq.cons line lines'), lines)
      | None -> None)
    (input_lines chan)
