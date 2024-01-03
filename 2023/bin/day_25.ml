open Advent

module Config = struct
  type t = (string, string) Hashtbl.t

  let add cfg a b = Hashtbl.add cfg (min a b) (max a b)
  let mem cfg a b = Hashtbl.find_all cfg (min a b) |> List.mem (max a b)

  let of_lines lines =
    let cfg = Hashtbl.create 1000 in
    Seq.iter
      (fun line ->
        match String.split_on_char ':' line with
        | [ a; bs ] ->
            String.split_on_char ' ' bs |> List.map String.trim
            |> List.filter (( <> ) "")
            |> List.iter (add cfg a)
        | _ -> invalid_arg __FUNCTION__)
      lines;
    cfg

  let pp fmt cfg =
    Format.(
      pp_print_string fmt "graph {\n";
      Hashtbl.iter (fun k v -> fprintf fmt "\t%s -- %s;\n" k v) cfg;
      pp_print_string fmt "}\n")
end
