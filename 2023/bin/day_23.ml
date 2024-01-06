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

  let pp ?highlight =
    Grid.pp ?highlight (fun fmt tile -> Format.pp_print_string fmt (Tile.to_string tile))
end

module Graph = struct
  let vertices trails =
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

  let edges trails vertices =
    let size = Grid.size trails in
    let ans = Hashtbl.create 1024 in
    Pos_Set.iter
      (fun start ->
        let rec loop path pos =
          if pos <> start && Pos_Set.mem pos vertices then
            Hashtbl.add ans start (pos, Pos_Set.cardinal path - 1)
          else
            (match trails.@(pos) with
            | Tile.Path | Tile.Slope _ -> Dir.[ Up; Left; Right; Down ]
            | Tile.Forest -> [])
            |> List.map Pos.(add pos % of_dir)
            |> List.filter Pos.(is_valid size)
            |> List.filter (( <> ) Tile.Forest % Grid.get trails)
            |> List.filter (Pos_Set.is_not_mem path)
            |> List.iter (fun next -> loop (Pos_Set.add next path) next)
        in
        loop Pos_Set.(singleton start) start)
      vertices;
    ans

  let of_trail_map trails =
    let v = vertices trails in
    let g = edges trails v in
    (* Each undirected edge was added twice (from u to v, and from v
       to u), deduplicate. *)
    Hashtbl.filter_map_inplace (fun u (v, w) -> if u < v then Some (v, w) else None) g;
    Printf.(
      eprintf "// V = %d\n%!" (Pos_Set.cardinal v);
      eprintf "// E = %d\n%!" (Hashtbl.length g));
    g

  let pp fmt g =
    Format.(
      pp_print_string fmt "graph {\n";
      Hashtbl.fold (fun u (v, _) set -> Pos_Set.(add u (add v set))) g Pos_Set.empty
      |> Pos_Set.iter (fun (row, col) ->
             fprintf fmt "\t\"(%d,%d)\" [pos=\"%d,%d!\"];\n" row col col (-row));
      Hashtbl.iter
        (fun u (v, w) ->
          fprintf fmt "\t\"(%d,%d)\" -- \"(%d,%d)\" [label=\"%d\"];\n" (fst u) (snd u) (fst v)
            (snd v) w)
        g;
      pp_print_string fmt "}";
      pp_print_newline fmt ())
end

let run ~slippery =
  let trails = Trail_Map.of_lines (input_lines stdin) in
  let start, finish = Trail_Map.(start trails, finish trails) in
  Format.(Graph.of_trail_map trails |> Graph.pp err_formatter);
  let hike = Option.get (Trail_Map.longest_hike ~slippery trails start finish) in
  Format.(Trail_Map.pp ~highlight:(Pos_Set.to_list hike) err_formatter trails);
  print_endline (string_of_int (Pos_Set.cardinal hike))
