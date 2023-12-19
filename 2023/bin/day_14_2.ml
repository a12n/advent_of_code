open Advent
open Day_14
module Platform_Hash = Hashtbl.Make (Platform)

let () =
  let plat = Platform.of_lines (input_lines stdin) in
  let seen = Platform_Hash.create 256 in
  let rec run i n =
    if i < n then (
      Printf.eprintf "@ %d ~ %d\n%!" i n;
      Platform.tilt_cycle_inplace plat;
      match Platform_Hash.find_opt seen plat with
      | Some k ->
        Printf.eprintf "@ %d = @ %d ~ n%!" i k;
        Platform_Hash.reset seen;
        run (n - ((n - i) mod (i - k)) + 1) n
      | None ->
        Platform_Hash.replace seen (Platform.copy plat) i;
        run (i + 1) n)
  in
  run 0 1_000_000_000;
  print_endline (string_of_int (Platform.load plat))
