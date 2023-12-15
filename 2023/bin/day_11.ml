open Advent

module Image = struct
  type t = (int * int) list

  let expand ?(factor = 1) img =
    let diff dim img =
      0
      :: (List.map dim img |> List.diffs ( - )
         |> List.map (function 0 -> 0 | 1 -> 0 | d -> (d - 1) * factor)
         |> List.fold_left_map (fun sum d -> (sum + d, sum + d)) 0
         |> snd)
    in
    let by dim a b = Int.compare (dim a) (dim b) in
    let img = List.stable_sort (by snd) img in
    let img = List.map2 (fun (row, col) dx -> (row, col + dx)) img (diff snd img) in
    let img = List.stable_sort (by fst) img in
    let img = List.map2 (fun (row, col) dy -> (row + dy, col)) img (diff fst img) in
    img

  let dist (row1, col1) (row2, col2) = Int.(abs (row2 - row1) + abs (col2 - col1))

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
