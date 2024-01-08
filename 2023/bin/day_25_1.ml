open Advent
open Day_25

let () =
  let conns = of_lines (input_lines stdin) in
  Graph.pp Format.err_formatter conns;
  (* Identified from Graphviz visualization. *)
  let conns =
    List.fold_left
      (fun conns (u, v) -> Graph.remove_edge conns u v ())
      conns
      [ ("hqq", "xxq"); ("xzz", "kgl"); ("vkd", "qfb") ]
  in
  Graph.pp Format.err_formatter conns;
  Graph.components conns
  |> List.map (List.length % Graph.vertices)
  |> List.reduce ( * ) |> string_of_int |> print_endline
