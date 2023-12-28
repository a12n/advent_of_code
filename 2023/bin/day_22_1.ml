open Advent
open Day_22

let () =
  let bricks = Snapshot.of_lines (input_lines stdin) in
  let nx, ny, nz = Snapshot.grid_size bricks in
  Printf.printf "%d %d %d\n" nx ny nz;
  let bricks = Snapshot.sort bricks in
  let bricks, _ = Snapshot.settle bricks in
  List.iter
    (fun (Point.{ x = x1; y = y1; z = z1 }, Point.{ x = x2; y = y2; z = z2 }) ->
      Printf.printf "%d %d %d %d %d %d\n" x1 y1 z1 x2 y2 z2)
    bricks
