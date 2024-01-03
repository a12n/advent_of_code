open Advent
open Day_22
open Hashtbl.Ops

let () =
  let bricks = Snapshot.(settle (of_lines (input_lines stdin))) in
  let supports = Hashtbl.create 256 in
  let disintegrate =
    List.fold_left
      (fun disintegrate (i, (_, support)) ->
        Int_Set.iter
          (fun j -> Hashtbl.modify ~default:(Int_Set.singleton i) supports (Int_Set.add i) j)
          support;
        if Int_Set.cardinal support = 1 then Int_Set.union disintegrate support else disintegrate)
      Int_Set.empty
      (List.mapi (fun i b -> (i, b)) bricks)
  in
  Format.(
    pp_print_string err_formatter "Disintegrate:";
    Int_Set.iter (fprintf err_formatter " %d") disintegrate;
    pp_print_newline err_formatter ());
  Format.(
    pp_print_string err_formatter "Supports:\n";
    Hashtbl.iter
      (fun k v ->
        fprintf err_formatter "\t%d:" k;
        Int_Set.iter (fprintf err_formatter " %d") v;
        pp_print_newline err_formatter ())
      supports);
  let rec collect i =
    match supports.%%{i} with
    | Some supported ->
        Format.(
          fprintf err_formatter "Brick %d supports %d bricks\n%!" i (Int_Set.cardinal supported));
        Int_Set.fold (fun j ans -> Int_Set.union ans (collect j)) supported supported
    | None -> Int_Set.empty
  in
  let other =
    Int_Set.fold
      (fun i total -> total + Int_Set.cardinal (Int_Set.diff (collect i) disintegrate))
      disintegrate 0
  in
  print_endline (string_of_int other)
