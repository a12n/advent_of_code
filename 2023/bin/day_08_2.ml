open Day_08

let () =
  let dirs, nodes = input stdin in
  let srcs, network =
    Seq.fold_left
      (fun (srcs, network) (from, dest) ->
         ( (if ID.is_part_source from then from :: srcs else srcs),
           Network.add network from dest ))
      ([], Network.make ())
      nodes
  in
  let rec loop node i =
    if List.for_all ID.is_part_dest node then i
    else
      let dir = dirs.(i mod Array.length dirs) in
      let node' = List.map (Network.next network dir) node in
      loop node' (i + 1)
  in
  print_endline (string_of_int (loop srcs 0))
