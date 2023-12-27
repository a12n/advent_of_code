open Advent

module Garden = struct
  type t = char array array

  let start =
    Array.find_mapi (fun row line ->
        Option.map
          (fun col -> (row, col))
          (Array.find_mapi (fun col plot -> if plot = 'S' then Some col else None) line))

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % String.to_seq)

  let pp fmt =
    Format.(
      Array.iter (fun line ->
          Array.iter
            (pp_print_string fmt % function
             | '.' -> "."
             | '#' -> "▓"
             | 'S' -> "@"
             | _ -> failwith __FUNCTION__)
            line;
          pp_print_newline fmt ()))
end
