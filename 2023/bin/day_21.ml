open Advent

module Garden = struct
  type t = char array array

  let start =
    Array.find_mapi (fun row line ->
        Option.map
          (fun col -> (row, col))
          (Array.find_mapi (fun col plot -> if plot = 'S' then Some col else None) line))

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % String.to_seq)
end
