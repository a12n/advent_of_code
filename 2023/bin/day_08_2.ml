open Advent
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
  (* XXX: Assume it's a bijection from source nodes to destination
     nodes. It's the case for the puzzle input, but may not be true in
     some other case. *)
  (* For each source node, find the number of steps to a first destination. *)
  List.map (Network.path network ID.is_part_dest dirs 0) srcs
  (* Ignore destination identifiers, keep only number of steps. *)
  |> List.map fst
  (* Find least common multiple of these number of steps. *)
  |> List.reduce Int.lcm
  (* Print the result. *)
  |> string_of_int
  |> print_endline
