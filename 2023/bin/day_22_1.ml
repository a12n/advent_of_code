open Advent
open Day_22

let () =
  (* List of settled bricks along with a set of brick numbers
     supporting each brick. *)
  let bricks, supports, _ = Snapshot.(settle (of_lines (input_lines stdin))) in
  List.iteri
    (fun i (Point.{ x = x1; y = y1; z = z1 }, Point.{ x = x2; y = y2; z = z2 }) ->
      Printf.eprintf "%d,%d,%d~%d,%d,%d\n%!" x1 y1 z1 x2 y2 z2;
      Printf.eprintf "# Brick %d supported by:" i;
      List.iter (Printf.eprintf " %d") (Hashtbl.find_all supports i);
      Printf.eprintf "\n%!")
    bricks;
  let unsafe = Snapshot.unsafe bricks supports in
  Printf.eprintf "# Unsafe bricks %d/%d\n%!" (Int_Set.cardinal unsafe) (List.length bricks);
  print_endline (string_of_int (List.length bricks - Int_Set.cardinal unsafe))
