open Advent

let ( .@[] ) grid (row, col) = grid.(row).(col)
let ( .@[]<- ) grid (row, col) value = grid.(row).(col) <- value

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
  type t = Plot.t array array

  let start =
    Array.find_mapi (fun row line ->
        Option.map
          (fun col -> (row, col))
          (Array.find_mapi (fun col plot -> if plot = Plot.Start then Some col else None) line))

  let steps garden pos n =
    let ((n_rows, n_cols) as size) = Array.matrix_size garden in
    if not (Pos.is_valid size pos) then invalid_arg __FUNCTION__;
    let state = Array.make_matrix n_rows n_cols [] in
    state.@[pos] <- [ 0 ];
    for i = 0 to n do
      for row = 0 to n_rows - 1 do
        for col = 0 to n_cols - 1 do
          if garden.(row).(col) <> Plot.Rock && List.mem i state.(row).(col) then
            Dir.[ Up; Left; Right; Down ]
            |> List.map (Pos.add (row, col) % Dir.to_pos)
            |> List.filter (Pos.is_valid size)
            |> List.filter (fun pos -> garden.@[pos] <> Plot.Rock)
            |> List.iter (fun pos -> state.@[pos] <- (i + 1) :: state.@[pos])
        done
      done
    done;
    Array.fold_lefti
      (fun reachable row line ->
        Array.fold_lefti
          (fun reachable col nums -> if List.mem n nums then (row, col) :: reachable else reachable)
          reachable line)
      [] state

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map Plot.of_char % String.to_seq)

  let pp ?(highlight = []) fmt =
    Format.(
      Array.iteri (fun row line ->
          Array.iteri
            (fun col plot ->
              let on, off =
                if List.mem (row, col) highlight then ("\x1b[42m", "\x1b[0m") else ("", "")
              in
              pp_print_string fmt (on ^ Plot.to_string plot ^ off))
            line;
          pp_print_newline fmt ()))
end
