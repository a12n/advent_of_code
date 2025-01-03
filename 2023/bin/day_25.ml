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
