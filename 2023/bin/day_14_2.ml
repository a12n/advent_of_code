open Advent
open Day_14

module Platform_Hash = Hashtbl.Make (Platform)

let () =
  let p = Platform.of_lines (input_lines stdin) in
  let seen = Platform_Hash.create 100 in (* TODO *)
  for cycles = 1 to 1_000_000_000 do
    Platform.tilt_cycle_inplace p;
    Printf.eprintf "cycles %d, hash %d\n%!" cycles (Platform.hash p);
    match Platform_Hash.find_opt seen p with
    | Some k -> Printf.eprintf "After %d cycles is the same as after %d cycles\n%!" cycles k
    | None -> Platform_Hash.replace seen p cycles
  done;
  print_endline (string_of_int (Platform.load p))
