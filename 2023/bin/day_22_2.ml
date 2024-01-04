open Advent
open Day_22

let () =
  let bricks, supports, supported = Snapshot.(settle (of_lines (input_lines stdin))) in
  let unsafe = Snapshot.unsafe bricks supports in
  let disintegrate i =
    let queue = Queue.create () in
    let affected = ref (Int_Set.singleton i) in
    Queue.add i queue;
    while not (Queue.is_empty queue) do
      let i = Queue.take queue in
      affected :=
        List.fold_left
          (fun affected j ->
            match Hashtbl.find_all supports j with
            | [] -> failwith __FUNCTION__
            | l ->
                if List.for_all (Int_Set.is_mem affected) l then (
                  Queue.add j queue;
                  Int_Set.add j affected)
                else affected)
          !affected (Hashtbl.find_all supported i)
    done;
    !affected
  in
  Int_Set.to_list unsafe
  |> List.map (fun i ->
         let affected = disintegrate i in
         Printf.eprintf "disintegrate %d, affected %d\n%!" i (Int_Set.cardinal affected - 1);
         Int_Set.cardinal affected - 1)
  |> List.reduce ( + ) |> string_of_int |> print_endline
