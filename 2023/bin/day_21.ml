open Advent

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

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map Plot.of_char % String.to_seq)

  let pp fmt =
    Format.(
      Array.iter (fun line ->
          Array.iter (pp_print_string fmt % Plot.to_string) line;
          pp_print_newline fmt ()))
end
