open Advent

module Note = struct
  type t = Ash | Rock

  let of_char = function '.' -> Ash | '#' -> Rock | _ -> invalid_arg __FUNCTION__
end

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val reflection : t -> [ `Horiz of int | `Vert of int ] option
  val reflection2 : t -> [ `Horiz of int | `Vert of int ] option
end = struct
  type t = Note.t array array

  let of_lines =
    Array.of_seq
    % Seq.map (Array.of_seq % Seq.map Note.of_char % String.to_seq)
    % Seq.take_while (( <> ) "")

  let find_reflection ?(dist = 0) ?(skip = []) notes =
    let n_rows = Array.length notes in
    Seq.find_map
      (fun (row, len) ->
        let mid = row + (len / 2) in
        if List.mem mid skip then None
        else
          let total_dist =
            Seq.Symmetric.ints row len
            |> Seq.map (fun (i, j) -> Array.hamming_dist Stdlib.( = ) notes.(i) notes.(j))
            |> Seq.reduce ( + )
          in
          if total_dist > dist then None else Some mid)
      (Seq.Symmetric.windows n_rows)

  let reflection notes =
    match find_reflection notes with
    | Some row -> Some (`Horiz row)
    | None -> (
        match find_reflection (Array.transpose notes) with
        | Some col -> Some (`Vert col)
        | None -> None)

  let reflection2 notes =
    let aux matrix =
      let n_rows = Array.length matrix in
      Seq.find
        (fun (row, len) ->
          let total_dist =
            Seq.Symmetric.ints row len
            |> Seq.map (fun (i, j) -> Array.hamming_dist Stdlib.( = ) matrix.(i) matrix.(j))
            |> Seq.reduce ( + )
          in
          total_dist < 2)
        (Seq.Symmetric.windows n_rows)
      |> Option.map (fun (row, len) -> row + (len / 2))
    in
    match Array.symmetry (Array.equal ( = )) notes with
    | Some _ -> (
        match aux (Array.transpose notes) with Some col -> Some (`Vert col) | None -> None)
    | None -> ( match aux notes with Some row -> Some (`Horiz row) | None -> None)
end

let input chan =
  Seq.unfold
    (fun lines ->
      match Seq.uncons lines with
      | Some (line, lines') -> Some (Grid.of_lines (Seq.cons line lines'), lines)
      | None -> None)
    (input_lines chan)
