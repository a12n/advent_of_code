open Advent
open Day_22

let () =
  (* List of settled bricks along with a set of brick numbers
     supporting each brick. *)
  let bricks = Snapshot.(settle (of_lines (input_lines stdin))) in
  List.iteri
    (fun i ((Point.{ x = x1; y = y1; z = z1 }, Point.{ x = x2; y = y2; z = z2 }), support) ->
      Printf.eprintf "%d,%d,%d~%d,%d,%d\n%!" x1 y1 z1 x2 y2 z2;
      Printf.eprintf "# Brick %d supported by:" i;
      Int_Set.iter (Printf.eprintf " %d") support;
      Printf.eprintf "\n%!")
    bricks;
  let disintegrate =
    List.fold_left
      (fun candidates (_, support) ->
        if Int_Set.cardinal support = 1 then Int_Set.diff candidates support else candidates)
      (Int_Set.of_list List.(init (length bricks) Fun.id))
      bricks
  in
  Printf.eprintf "# Disintegrate:";
  Int_Set.iter (Printf.eprintf " %d") disintegrate;
  Printf.eprintf "\n%!";
  print_endline (string_of_int (Int_Set.cardinal disintegrate))
