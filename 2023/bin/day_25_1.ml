open Advent
open Day_25

let () =
  let conns = of_lines (input_lines stdin) in
  Format.(
    fprintf err_formatter "// Initial graph:\n";
    Graph.pp err_formatter conns);
  let cut =
    match List.length (Graph.vertices conns) with
    | 15 ->
        (* Sample input. *)
        [ ("hfx", "pzl"); ("bvb", "cmg"); ("nvd", "jqt") ]
    | 1435 ->
        (* Puzzle input, edges identified from Graphviz visualization. *)
        [ ("hqq", "xxq"); ("xzz", "kgl"); ("vkd", "qfb") ]
    | n -> failwith (Printf.sprintf "unknown graph with %d vertices" n)
  in
  let conns' = List.fold_left (fun conns (u, v) -> Graph.remove_edge conns u v ()) conns cut in
  Format.(
    fprintf err_formatter "// After cut:\n";
    Graph.pp err_formatter conns');
  Graph.components conns'
  |> List.mapi (fun i cc ->
         Format.(
           fprintf err_formatter "// Connected component %d:\n" i;
           Graph.pp err_formatter cc);
         cc)
  |> List.map (List.length % Graph.vertices)
  |> List.reduce ( * ) |> string_of_int |> print_endline
