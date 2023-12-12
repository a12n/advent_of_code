open Day_08

let () =
  let dirs, network = input stdin in
  Printf.eprintf "dirs %d\n%!" (Array.length dirs);
  let dest = ID.of_string "ZZZ" in
  let rec loop node i =
    if node <> dest then
      loop (Network.next network node dirs.(i mod Array.length dirs)) (i + 1)
    else i
  in
  let num_steps = loop (ID.of_string "AAA") 0 in
  print_endline (string_of_int num_steps)
