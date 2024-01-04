open Advent
open Day_22

let () =
  (* Settle bricks. *)
  let bricks, supports, supported = Snapshot.(settle (of_lines (input_lines stdin))) in
  (* Define disintegrate function in terms of supports/supportedmappings. *)
  let disintegrate id =
    let rec loop affected id1 =
      List.fold_left
        (fun affected id2 ->
          match Hashtbl.find_all supports id2 with
          | [] -> failwith __FUNCTION__
          | l ->
              if List.for_all (Int_Set.is_mem affected) l then loop (Int_Set.add id2 affected) id2
              else affected)
        affected (Hashtbl.find_all supported id1)
    in
    (* Find affected bricks, remove the disintegrated brick from the
       set of affected bricks (the problem asks about number of other
       bricks, besides the disintegrated one. *)
    Int_Set.remove id (loop (Int_Set.singleton id) id)
  in
  (* Consider only unsafe bricks, other bricks wouldn't affect anything. *)
  Int_Set.to_seq (Snapshot.unsafe bricks supports)
  (* Disintegrate the brick, find out the cardinality of the set of
     other affected bricks. *)
  |> Seq.map (Int_Set.cardinal % disintegrate)
  (* Sum and print. *)
  |> Seq.reduce ( + )
  |> string_of_int |> print_endline
