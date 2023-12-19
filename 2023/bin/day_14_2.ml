open Advent
open Day_14

let () =
  let p = Platform.of_lines (input_lines stdin) in
  let _seen = Hashtbl.create 8 in (* TODO *)
  for cycles = 1 to 1000000000 do
    Platform.tilt_cycle_inplace p;
    Printf.eprintf "cycles %d\n%!" cycles
  done;
  print_endline (string_of_int (Platform.load p))
