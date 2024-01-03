open Advent
open Grid.Ops
module Pos = Grid.Pos

module Pos_Set = struct
  include Set.Make (Pos)

  let is_mem = Fun.flip mem
  let is_not_mem set elt = not (is_mem set elt)
end

module Tile = struct
  type t = Path | Forest | Slope of Dir.t

  let of_char = function '.' -> Path | '#' -> Forest | c -> Slope (Dir.of_char c)
  let to_char = function Path -> '.' | Forest -> '#' | Slope dir -> Dir.to_char dir
  let to_string = function Path -> "." | Forest -> "█" | Slope dir -> Dir.to_string dir
end

module Trail_Map = struct
  type t = Tile.t Grid.t

  let start trails = (0, Option.get (Array.find_index (( = ) Tile.Path) trails.(0)))

  let finish trails =
    let row = Array.length trails - 1 in
    (row, Option.get (Array.find_index (( = ) Tile.Path) trails.(row)))

  let longest_hike ?(slippery = true) trails start finish =
    let size = Grid.size trails in
    let rec loop seen pos =
      if pos = finish then Some (Pos_Set.add pos seen)
      else
        (match trails.@(pos) with
        | Tile.Path -> Dir.[ Up; Left; Right; Down ]
        | Tile.Slope dir -> if slippery then [ dir ] else Dir.[ Up; Left; Right; Down ]
        | Tile.Forest -> failwith __FUNCTION__)
        |> List.map Pos.(add pos % of_dir)
        |> List.filter Pos.(is_valid size)
        |> List.filter (( <> ) Tile.Forest % Grid.get trails)
        |> List.filter Pos_Set.(is_not_mem seen)
        |> List.filter_map (fun pos -> loop (Pos_Set.add pos seen) pos)
        |> List.reduce_opt (fun longest hike ->
               if Pos_Set.(cardinal hike > cardinal longest) then hike else longest)
    in
    loop Pos_Set.empty start

  let of_lines = Grid.of_lines (fun _ -> Tile.of_char)
  let pp = Grid.pp (fun fmt tile -> Format.pp_print_string fmt (Tile.to_string tile))
end

let run ~slippery =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  let start, finish = Trail_Map.(start trails, finish trails) in
  Format.(
    Trail_Map.pp err_formatter trails;
    Pos.pp err_formatter start;
    pp_print_string err_formatter " -> ";
    Pos.pp err_formatter finish;
    pp_print_newline err_formatter ());
  let hike = Option.get (Trail_Map.longest_hike ~slippery trails start finish) in
  print_endline (string_of_int (Pos_Set.cardinal hike))
