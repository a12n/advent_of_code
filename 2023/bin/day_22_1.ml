open Advent
open Day_22

let () =
  let bricks = Snapshot.of_lines (input_lines stdin) in
  let _, Point.{ x = nx; y = ny; z = nz } = Snapshot.grid_span bricks in
  Printf.printf "%d %d %d\n" (nx + 1) (ny + 1) (nz + 1);
  let bricks = Snapshot.sort bricks in
  let bricks, _ = Snapshot.settle bricks in
  List.iter
    (fun (Point.{ x = x1; y = y1; z = z1 }, Point.{ x = x2; y = y2; z = z2 }) ->
      Printf.printf "%d %d %d %d %d %d\n" x1 y1 z1 x2 y2 z2)
    bricks
