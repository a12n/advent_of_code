open Advent
open Grid.Ops
module Pos = Grid.Pos

module Heat_Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val heat_loss : ?min_straight:int -> ?max_straight:int -> t -> Pos.t -> Pos.t -> int
  val size : t -> int * int
end = struct
  type t = int Grid.t

  let size = Grid.size

  module Min_Queue = Set.Make (struct
    type t = int * Pos.t * Dir.t option * int

    let compare = Stdlib.compare
  end)

  let heat_loss ?(min_straight = 0) ?(max_straight = max_int) grid src dest =
    let size = Grid.size grid in
    let seen = Hashtbl.create (fst size * snd size) in
    let rec loop queue =
      let ((loss, pos, dir, steps) as elt) = Min_Queue.min_elt queue in
      (* Printf.eprintf "loss %d, pos (%d, %d), dir %s, straight %d\n%!" loss (fst pos) (snd pos) *)
      (*   (Option.fold ~none:"_" ~some:Dir.to_string dir) *)
      (*   straight; *)
      let queue' = Min_Queue.remove elt queue in
      if pos = dest && steps >= min_straight then
        (* Printf.eprintf "reached (%d, %d)\n%!" (fst dest) (snd dest); *)
        loss
      else if Hashtbl.mem seen (pos, dir, steps) then loop queue'
      else (
        Hashtbl.replace seen (pos, dir, steps) ();
        (match dir with
        | Some dir ->
            List.concat
              [
                (if steps < max_straight then [ (dir, steps + 1) ] else []);
                (if steps >= min_straight then Dir.[ (turn_left dir, 1); (turn_right dir, 1) ]
                 else []);
              ]
        | None -> Dir.[ (Down, 1); (Right, 1); (Left, 1); (Up, 1) ])
        |> List.filter_map (fun (dir, steps) ->
               let pos' = Grid.Pos.(add pos (of_dir dir)) in
               (* Printf.eprintf "next: pos' (%d, %d), ok %B\n%!" (fst pos') (snd pos') *)
               (*   (Pos.is_valid size pos'); *)
               if Grid.Pos.is_valid size pos' then Some (loss + grid.@(pos'), pos', Some dir, steps)
               else None)
        |> List.fold_left (Fun.flip Min_Queue.add) queue'
        |> loop)
    in
    loop (Min_Queue.singleton (0, src, None, 0))

  let of_lines = Grid.of_lines (fun _ c -> int_of_char c - int_of_char '0')
end
