open Advent
open Day_14

module Platform_Hash = Hashtbl.Make (Platform)

let () =
  let p = Platform.of_lines (input_lines stdin) in
  let _seen = Platform_Hash.create 10 in (* TODO *)
  for cycles = 1 to 1_000_000_000 do
    Platform.tilt_cycle_inplace p;
    Printf.eprintf "cycles %d\n%!" cycles
  done;
  print_endline (string_of_int (Platform.load p))
