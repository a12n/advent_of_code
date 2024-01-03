open Advent

module Tile = struct
  type t = Path | Forest | Slope of Dir.t

  let of_char = function '.' -> Path | '#' -> Forest | c -> Slope (Dir.of_char c)
  let to_char = function Path -> '.' | Forest -> '#' | Slope dir -> Dir.to_char dir
  let to_string = function Path -> "." | Forest -> "#" | Slope dir -> Dir.to_string dir
end

module Trail_Map = struct
  type t = Tile.t Grid.t

  let of_lines = Grid.of_lines (fun _ -> Tile.of_char)
  let pp = Grid.pp (fun fmt tile -> Format.pp_print_string fmt (Tile.to_string tile))
end
