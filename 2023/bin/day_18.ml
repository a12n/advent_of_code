open Advent

module Dig = struct
  type t = { dir : Dir.t; length : int; color : int }

  let of_string s =
    Scanf.sscanf s " %c %d (#%06x) " (fun dir length color ->
        {
          dir =
            (match dir with
            | 'U' -> Dir.Up
            | 'L' -> Dir.Left
            | 'R' -> Dir.Right
            | 'D' -> Dir.Down
            | _ -> invalid_arg __FUNCTION__);
          length = (if length > 0 then length else invalid_arg __FUNCTION__);
          color;
        })
end

module Plan = struct
  type t = Dig.t list

  let of_lines = List.of_seq % Seq.map Dig.of_string
end
