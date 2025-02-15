module type VERTEX = sig
  type t

  val compare : t -> t -> int
end

module type WEIGHT = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type vertex
  type weight
  type t

  val empty : t
  val add_edge : t -> vertex -> vertex -> weight -> t
  val replace_edge : t -> vertex -> vertex -> weight -> t
  val remove_edge : t -> vertex -> vertex -> weight -> t
  val adjacent : t -> vertex -> (vertex * weight) list
  val edges : t -> (vertex * vertex * weight) list
  val vertices : t -> vertex list

  val pp :
    ([ `Attr | `Edge ] -> Format.formatter -> vertex -> unit) ->
    (Format.formatter -> weight -> unit) ->
    Format.formatter ->
    t ->
    unit
end

let pp pp_vertex pp_weight fmt dir vertices edges =
  Format.(
    pp_print_newline fmt ();
    fprintf fmt "// V = %d\n" (List.length vertices);
    fprintf fmt "// E = %d\n" (List.length edges);
    (match dir with
    | `Directed -> pp_print_string fmt "digraph {\n"
    | `Undirected ->
        pp_print_string fmt "graph {\n";
        pp_print_string fmt "\tgraph [layout=neato];\n");
    pp_print_newline fmt ();
    List.iter
      (fun u ->
        pp_print_char fmt '\t';
        pp_vertex `Attr fmt u;
        pp_print_char fmt ';';
        pp_print_newline fmt ())
      vertices;
    pp_print_newline fmt ();
    List.iter
      (fun (u, v, w) ->
        pp_print_char fmt '\t';
        pp_vertex `Edge fmt u;
        pp_print_string fmt (match dir with `Directed -> " -> " | `Undirected -> " -- ");
        pp_vertex `Edge fmt v;
        pp_print_string fmt " [label=\"";
        pp_weight fmt w;
        pp_print_string fmt "\"];";
        pp_print_newline fmt ())
      edges;
    pp_print_string fmt "}";
    pp_print_newline fmt ())

module Make_Directed (Vertex : VERTEX) (Weight : WEIGHT) = struct
  module Vertex_Map = Map.Make (Vertex)

  type t = Weight.t list Vertex_Map.t Vertex_Map.t

  let empty = Vertex_Map.empty

  let update_edges g u v f =
    Vertex_Map.update u
      (function
        | None -> ( match f [] with [] -> None | ws -> Some (Vertex_Map.singleton v ws))
        | Some adj ->
            let adj' =
              Vertex_Map.update v
                (function
                  | None -> ( match f [] with [] -> None | ws' -> Some ws')
                  | Some ws -> ( match f ws with [] -> None | ws' -> Some ws'))
                adj
            in
            if Vertex_Map.is_empty adj' then None else Some adj')
      g

  let add_edge g u v w = update_edges g u v (List.cons w)

  let replace_edge g u v w =
    update_edges g u v (function [] | [ _ ] -> [ w ] | _ :: ws -> w :: ws)

  let remove_edge g u v w = update_edges g u v (List.remove_if (fun w' -> Weight.compare w w' = 0))

  let adjacent g u =
    match Vertex_Map.find_opt u g with
    | None -> []
    | Some adj ->
        Vertex_Map.fold (fun v ws ans -> List.fold_left (fun ans w -> (v, w) :: ans) ans ws) adj []

  let edges g =
    Vertex_Map.fold
      (fun u adj ans ->
        Vertex_Map.fold
          (fun v ws ans -> List.fold_left (fun ans w -> (u, v, w) :: ans) ans ws)
          adj ans)
      g []

  let vertices g =
    Vertex_Map.fold (fun u adj ans -> Vertex_Map.fold (fun v _ws ans -> u :: v :: ans) adj ans) g []
    |> List.sort_uniq Vertex.compare

  let pp pp_vertex pp_weight fmt g = pp pp_vertex pp_weight fmt `Directed (vertices g) (edges g)
end

module Make_Undirected (Vertex : VERTEX) (Weight : WEIGHT) = struct
  module Directed = Make_Directed (Vertex) (Weight)

  type t = Directed.t

  let empty = Directed.empty
  let add_edge g u v w = Directed.(add_edge (add_edge g u v w) v u w)
  let replace_edge g u v w = Directed.(replace_edge (replace_edge g u v w) v u w)
  let remove_edge g u v w = Directed.(remove_edge (remove_edge g u v w) v u w)
  let adjacent = Directed.adjacent
  let edges g = Directed.edges g |> List.filter (fun (u, v, _) -> Vertex.compare u v < 0)
  let vertices = Directed.vertices

  let components g =
    let vertex2id = Hashtbl.create (Directed.Vertex_Map.cardinal g) in
    let id2vertex = Hashtbl.create 16 in
    let rec dfs id u =
      if Hashtbl.mem vertex2id u then ()
      else (
        Hashtbl.add vertex2id u id;
        Hashtbl.add id2vertex id u;
        List.iter (dfs id) (List.map fst (adjacent g u)))
    in
    ignore
      (List.fold_left
         (fun id u ->
           dfs id u;
           id + 1)
         0 (vertices g));
    let id2cc = Hashtbl.create 16 in
    Hashtbl.iter
      (fun id u ->
        Hashtbl.find_opt id2cc id
        |> Option.value ~default:Directed.Vertex_Map.empty
        |> Directed.Vertex_Map.(add u (find u g))
        |> Hashtbl.replace id2cc id)
      id2vertex;
    List.of_seq (Hashtbl.to_seq_values id2cc)

  let pp pp_vertex pp_weight fmt g = pp pp_vertex pp_weight fmt `Undirected (vertices g) (edges g)
end
