open Advent
open Grid.Ops

module Plot = struct
  type t = Garden | Rock | Start

  let of_char = function
    | '.' -> Garden
    | '#' -> Rock
    | 'S' -> Start
    | _ -> invalid_arg __FUNCTION__

  let to_string = function Garden -> "." | Rock -> "▓" | Start -> "@"
end

module Garden = struct
  type t = Plot.t Grid.t

  let start = Grid.find_pos (( = ) Plot.Start)

  let steps garden pos n =
    let ((n_rows, n_cols) as size) = Grid.size garden in
    if not (Grid.Pos.is_valid size pos) then invalid_arg __FUNCTION__;
    let state = Array.make_matrix n_rows n_cols [] in
    state.@(pos) <- [ 0 ];
    for i = 0 to n do
      for row = 0 to n_rows - 1 do
        for col = 0 to n_cols - 1 do
          if garden.(row).(col) <> Plot.Rock && List.mem i state.(row).(col) then
            Dir.[ Up; Left; Right; Down ]
            |> List.map (Grid.Pos.add (row, col) % Dir.to_pos)
            |> List.filter (Grid.Pos.is_valid size)
            |> List.filter (fun pos -> garden.@(pos) <> Plot.Rock)
            |> List.iter (fun pos -> state.@(pos) <- (i + 1) :: state.@(pos))
        done
      done
    done;
    Array.fold_lefti
      (fun reachable row line ->
        Array.fold_lefti
          (fun reachable col nums -> if List.mem n nums then (row, col) :: reachable else reachable)
          reachable line)
      [] state

  let of_lines = Grid.of_lines Plot.of_char

  let pp ?highlight =
    Grid.pp ?highlight (fun fmt plot -> Format.pp_print_string fmt (Plot.to_string plot))
end
