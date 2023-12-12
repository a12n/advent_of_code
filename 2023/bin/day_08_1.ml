open Day_08

let () =
  let dirs, nodes = input stdin in
  let network =
    Seq.fold_left
      (fun network (src, dest) -> Network.add network src dest)
      (Network.make ()) nodes
  in
  let num_steps, _ =
    Network.path network ID.is_dest dirs 0 (ID.of_string "AAA")
  in
  print_endline (string_of_int num_steps)
