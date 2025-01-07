open Advent

module Graph = struct
  include Graph.Make_Undirected (String) (Unit)

  let pp = pp (fun _ -> Format.pp_print_string) (fun _fmt () -> ())
end

let of_lines lines =
  Seq.fold_left
    (fun cfg line ->
      match String.split_on_char ':' line with
      | [ a; bs ] ->
          String.split_on_char ' ' bs |> List.map String.trim
          |> List.filter (( <> ) "")
          |> List.fold_left (fun cfg b -> Graph.replace_edge cfg a b ()) cfg
      | _ -> invalid_arg __FUNCTION__)
    Graph.empty lines

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
