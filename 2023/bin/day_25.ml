open Advent

let of_lines lines =
  let conns = Hashtbl.create 1000 in
  Seq.iter
    (fun line ->
      match String.split_on_char ':' line with
      | [ a; bs ] ->
          String.split_on_char ' ' bs |> List.map String.trim
          |> List.filter (( <> ) "")
          |> List.iter (fun b ->
                 Hashtbl.(
                   add conns a b;
                   add conns b a))
      | _ -> invalid_arg __FUNCTION__)
    lines;
  conns
