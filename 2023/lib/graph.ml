open Fun.Ops

module type VERTEX = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : ?attr:bool -> Format.formatter -> t -> unit
end

module type WEIGHT = sig
  type t

  val zero : t
  val add : t -> t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type vertex
  type weight
  type t

  val make : int -> t
  val add_edge : t -> vertex -> vertex -> weight -> unit
  val replace_edge : t -> vertex -> vertex -> weight -> unit
  val adjacent : t -> vertex -> (vertex * weight) list
  val edges : t -> (vertex * vertex * weight) list
  val vertices : t -> vertex list
  val pp : Format.formatter -> t -> unit
end

let pp (type v w) (module V : VERTEX with type t = v) (module W : WEIGHT with type t = w) dir fmt
    vertices edges =
  Format.(
    pp_print_newline fmt ();
    fprintf fmt "// V = %d\n" (List.length vertices);
    fprintf fmt "// E = %d\n" (List.length edges);
    pp_print_string fmt (match dir with `Directed -> "digraph" | `Undirected -> "graph");
    pp_print_string fmt " {\n";
    List.iter
      (fun u ->
        pp_print_char fmt '\t';
        V.pp ~attr:true fmt u;
        pp_print_char fmt ';';
        pp_print_newline fmt ())
      vertices;
    pp_print_newline fmt ();
    List.iter
      (fun (u, v, w) ->
        pp_print_char fmt '\t';
        V.pp fmt u;
        pp_print_string fmt (match dir with `Directed -> " -> " | `Undirected -> " -- ");
        V.pp fmt v;
        pp_print_string fmt " [label=\"";
        W.pp fmt w;
        pp_print_string fmt "\"];";
        pp_print_newline fmt ())
      edges;
    pp_print_string fmt "}";
    pp_print_newline fmt ())

module Make_Directed (Vertex : VERTEX) (Weight : WEIGHT) = struct
  module Vertex_Table = Hashtbl.Make (Vertex)

  type t = Weight.t Vertex_Table.t Vertex_Table.t

  let make n = Vertex_Table.create n

  let adj_tbl g u =
    match Vertex_Table.find_opt g u with
    | Some tbl -> tbl
    | None ->
        let tbl = Vertex_Table.(create (length g)) in
        Vertex_Table.replace g u tbl;
        tbl

  let add_edge g u v w = Vertex_Table.add (adj_tbl g u) v w
  let replace_edge g u v w = Vertex_Table.replace (adj_tbl g u) v w

  let adjacent g u =
    Vertex_Table.find_opt g u |> Option.fold ~none:[] ~some:(List.of_seq % Vertex_Table.to_seq)

  let edges g =
    Vertex_Table.fold
      (fun u adj ans -> Vertex_Table.fold (fun v w ans -> (u, v, w) :: ans) adj ans)
      g []

  let vertices g =
    List.sort_uniq Vertex.compare
      (Vertex_Table.fold
         (fun u adj ans -> Vertex_Table.fold (fun v _ ans -> u :: v :: ans) adj ans)
         g [])

  let pp fmt g = pp (module Vertex) (module Weight) `Directed fmt (vertices g) (edges g)
end

module Make_Undirected (Vertex : VERTEX) (Weight : WEIGHT) = struct
  module Directed = Make_Directed (Vertex) (Weight)

  type t = Directed.t

  let make = Directed.make

  let add_edge g u v w =
    Directed.(
      add_edge g u v w;
      add_edge g v u w)

  let replace_edge g u v w =
    Directed.(
      replace_edge g u v w;
      replace_edge g v u w)

  let adjacent = Directed.adjacent
  let edges g = Directed.edges g |> List.filter (fun (u, v, _) -> u < v)
  let vertices = Directed.vertices
  let pp fmt g = pp (module Vertex) (module Weight) `Undirected fmt (vertices g) (edges g)
end
