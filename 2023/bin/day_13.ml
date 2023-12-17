open Advent

module Note = struct
  type t = Ash | Rock

  let of_char = function '.' -> Ash | '#' -> Rock | _ -> invalid_arg __FUNCTION__
end

module Grid = struct
  type t = Note.t array array

  let of_lines =
    Array.of_seq
    % Seq.map (Array.of_seq % Seq.map Note.of_char % String.to_seq)
    % Seq.take_while (( <> ) "")
end
