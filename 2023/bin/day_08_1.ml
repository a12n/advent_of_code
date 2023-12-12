open Day_08

let () =
  let dirs, nodes = input stdin in
  let network =
    Seq.fold_left
      (fun network (src, dest) -> Network.add network src dest)
      (Network.make ()) nodes
  in
  let dest = ID.of_string "ZZZ" in
  let rec loop node i =
    if node <> dest then
      loop (Network.next network dirs.(i mod Array.length dirs) node) (i + 1)
    else i
  in
  let num_steps = loop (ID.of_string "AAA") 0 in
  print_endline (string_of_int num_steps)
