(* https://en.wikipedia.org/wiki/Shoelace_formula *)
(* https://en.wikipedia.org/wiki/Pick%27s_theorem *)

let () =
  (* let path = [| *)
  (*   (1, 1); *)
  (*   (2, 1); *)
  (*   (3, 1); *)
  (*   (3, 2); *)
  (*   (3, 3); *)
  (*   (2, 3); *)
  (*   (1, 3); *)
  (*   (1, 2); *)
  (* |] in *)
  let path = [|
    3,1;
    1,1;
    1,6;
    6,6;
    6,4;
    3,4;
  |] in
  let area = ref 0 in
  for i = 0 to Array.length path - 1 do
    let (xi,yi) = path.(i) in
    let (xj,yj) = path.((i + 1) mod Array.length path) in
    (* area := !area + xi * yj - xj * yi *)
    area := !area + (yi + yj) * (xi - xj)
  done;
  (* area := !area / 2; *)
  print_endline (string_of_int !area)
