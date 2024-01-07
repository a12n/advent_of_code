module type S = sig
  type ('v, 'w) t

  val make : int -> ('v, 'w) t
  val add_edge : ('v, 'w) t -> 'v -> 'v -> 'w -> unit
  val adjacent : ('v, 'w) t -> 'v -> ('v * 'w) list
  val edges : ('v, 'w) t -> ('v * 'v * 'w) list
  val vertices : ('v, 'w) t -> 'v list

  val pp :
    vertex:([ `Attr | `Edge ] -> Format.formatter -> 'v -> unit) ->
    weight:(Format.formatter -> 'w -> unit) ->
    Format.formatter ->
    ('v, 'w) t ->
    unit
end

let pp pp_vertex pp_weight fmt dir vertices edges =
  Format.(
    pp_print_string fmt (match dir with `Directed -> "digraph" | `Undirected -> "graph");
    pp_print_string fmt " {\n";
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

module Directed = struct
  type ('v, 'w) t = ('v, 'v * 'w) Hashtbl.t

  let make n = Hashtbl.create n
  let add_edge g u v w = Hashtbl.add g u (v, w)
  let adjacent g u = Hashtbl.find_all g u
  let edges g = Hashtbl.fold (fun u (v, w) ans -> (u, v, w) :: ans) g []

  let vertices g =
    Hashtbl.fold (fun u (v, _) ans -> u :: v :: ans) g [] |> List.sort_uniq Stdlib.compare

  let pp ~vertex ~weight fmt g = pp vertex weight fmt `Directed (vertices g) (edges g)
end

module Undirected = struct
  type ('v, 'w) t = ('v, 'w) Directed.t

  let make = Directed.make

  let add_edge g u v w =
    Directed.(
      add_edge g u v w;
      add_edge g v u w)

  let adjacent = Directed.adjacent
  let edges g = Directed.edges g |> List.filter (fun (u, v, _) -> u < v)
  let vertices = Directed.vertices
  let pp ~vertex ~weight fmt g = pp vertex weight fmt `Undirected (vertices g) (edges g)
end
