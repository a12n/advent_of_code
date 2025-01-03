open Advent
open Grid.Ops
module Pos = Grid.Pos

module Pos_Set = struct
  include Set.Make (Pos)

  let is_mem = Fun.flip mem
  let is_not_mem set elt = not (is_mem set elt)
end

module Graph = struct
  include Graph.Make_Undirected (Pos) (Int)

  let pp =
    pp
      Format.(
        fun attr fmt (row, col) ->
          fprintf fmt "\"(%d,%d)\"" row col;
          if attr = `Attr then fprintf fmt " [pos=\"%d,%d!\"]" col (-row))
      Format.pp_print_int
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

  let pp ?highlight =
    Grid.pp ?highlight (fun fmt tile -> Format.pp_print_string fmt (Tile.to_string tile))

  let to_graph_vertices trails =
    let ((n_rows, _) as size) = Grid.size trails in
    Array.fold_lefti
      (fun ans row line ->
        Array.fold_lefti
          (fun ans col tile ->
            let dirs =
              List.filter
                (fun dir ->
                  let pos = Pos.(add (row, col) (of_dir dir)) in
                  Pos.is_valid size pos && trails.@(pos) <> Tile.Forest)
                (match tile with
                | Tile.Path | Tile.Slope _ -> Dir.[ Up; Left; Right; Down ]
                | Tile.Forest -> [])
            in
            if
              List.length dirs > 2
              || (dirs = [ Dir.Down ] && row = 0)
              || (dirs = [ Dir.Up ] && row = n_rows - 1)
            then Pos_Set.add (row, col) ans
            else ans)
          ans line)
      Pos_Set.empty trails

  let to_graph trails =
    let size = Grid.size trails in
    let vertices = to_graph_vertices trails in
    Pos_Set.fold
      (fun start ans ->
        let rec loop ans path pos =
          if pos <> start && Pos_Set.mem pos vertices then
            Graph.replace_edge ans start pos (Pos_Set.cardinal path - 1)
          else
            (match trails.@(pos) with
            | Tile.Path | Tile.Slope _ -> Dir.[ Up; Left; Right; Down ]
            | Tile.Forest -> [])
            |> List.map Pos.(add pos % of_dir)
            |> List.filter Pos.(is_valid size)
            |> List.filter (( <> ) Tile.Forest % Grid.get trails)
            |> List.filter (Pos_Set.is_not_mem path)
            |> List.fold_left (fun ans next -> loop ans (Pos_Set.add next path) next) ans
        in
        loop ans Pos_Set.(singleton start) start)
      vertices Graph.empty

  let longest_path graph start finish =
    let seen = Hashtbl.create 8192 in
    let rec loop dist u =
      if u = finish then Some dist
      else
        Graph.adjacent graph u
        |> List.filter (fun (v, _) -> not (Hashtbl.mem seen v))
        |> List.filter_map (fun (v, w) ->
               Hashtbl.replace seen v ();
               let ans = loop (dist + w) v in
               Hashtbl.remove seen v;
               ans)
        |> List.reduce_opt Int.max
    in
    loop 0 start
end
