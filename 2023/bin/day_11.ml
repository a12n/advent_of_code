open Advent

module Image = struct
  type t = (int * int) list

  let of_chars =
    List.rev % snd
    % Seq.fold_left
        (fun ((row, col), image) c ->
          match c with
          | '.' -> ((row, col + 1), image)
          | '\n' -> ((row + 1, 0), image)
          | '#' -> ((row, col + 1), (row, col) :: image)
          | _ -> invalid_arg __FUNCTION__)
        ((0, 0), [])
end
