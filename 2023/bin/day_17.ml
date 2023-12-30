open Advent
open Grid.Ops
module Pos = Grid.Pos

module Heat_Grid : sig
  type t

  val backtrack : Dir.t option Grid.t -> Pos.t -> Pos.t list
  val distance : t -> Pos.t -> int Grid.t * Dir.t option Grid.t
  val of_lines : string Seq.t -> t
  val heat_loss : ?min_straight:int -> ?max_straight:int -> t -> Pos.t -> Pos.t -> int
  val pp : ?path:Pos.t list -> Format.formatter -> t -> unit

  val pp_dist :
    ?path:Pos.t list -> Format.formatter -> t -> int Grid.t -> Dir.t option Grid.t -> unit

  val size : t -> int * int
end = struct
  type t = int Grid.t

  let cell_color path pos = if List.mem pos path then ("\x1b[32m", "\x1b[0m") else ("", "")
  let pp ?(path = []) = Grid.pp ~highlight:path ~sgr:"\x1b[32m" Format.pp_print_int

  let pp_dist ?(path = []) fmt grid dist dirs =
    let n_rows, n_cols = Array.matrix_size grid in
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        let dir = match dirs.(row).(col) with None -> "." | Some dir -> Dir.to_string dir in
        let color_on, color_off = cell_color path (row, col) in
        Format.fprintf fmt " %s[%s %1d %s]%s" color_on dir
          grid.(row).(col)
          (if dist.(row).(col) = max_int then "inf" else Printf.sprintf "%3d" dist.(row).(col))
          color_off
      done;
      Format.pp_print_newline fmt ()
    done

  let size = Grid.size

  module Priority_Queue = Set.Make (struct
    type t = int * (int * int)

    let compare = Stdlib.compare
  end)

  module Min_Queue = Set.Make (struct
    type t = int * Pos.t * Dir.t option * int

    let compare = Stdlib.compare
  end)

  let backtrack dirs dest =
    let rec loop path cur =
      Printf.eprintf "path %d, cur (%d, %d)\n%!" (List.length path) (fst cur) (snd cur);
      match dirs.@(cur) with
      | None -> cur :: path
      | Some dir ->
          let prev = Pos.(add cur (of_dir (Dir.neg dir))) in
          loop (cur :: path) prev
    in
    loop [] dest

  let distance grid src =
    let ((n_rows, n_cols) as size) = Grid.size grid in
    let dirs = Array.make_matrix n_rows n_cols None in
    let costs = Array.make_matrix n_rows n_cols max_int in
    let unfinished = Array.make_matrix n_rows n_cols true in
    let rec loop queue =
      let ((cost, cur) as elt) = Priority_Queue.min_elt queue in
      Printf.eprintf "cost %d, cur (%d, %d), queue %d\n%!" cost (fst cur) (snd cur)
        (Priority_Queue.cardinal queue);
      unfinished.@(cur) <- false;
      Dir.[ Up; Left; Right; Down ]
      |> List.map (fun dir -> (dir, Pos.(add cur (of_dir dir))))
      |> List.filter (fun (_, pos) -> Pos.is_valid size pos && unfinished.@(pos))
      |> List.fold_left
           (fun queue (dir, pos) ->
             let new_cost = cost + grid.@(pos) in
             if new_cost < costs.@(pos) then (
               costs.@(pos) <- new_cost;
               dirs.@(pos) <- Some dir;
               Priority_Queue.add (new_cost, pos) queue)
             else queue)
           (Priority_Queue.remove elt queue)
      |> loop
    in
    costs.@(src) <- 0;
    (try loop (Priority_Queue.singleton (0, src)) with Not_found -> ());
    (costs, dirs)

  let heat_loss ?(min_straight = 0) ?(max_straight = max_int) grid src dest =
    let size = Grid.size grid in
    if not Grid.Pos.(is_valid size src && is_valid size dest) then invalid_arg __FUNCTION__;
    let seen = Hashtbl.create (fst size * snd size) in
    let rec loop queue =
      let ((loss, pos, dir, straight) as elt) = Min_Queue.min_elt queue in
      (* Printf.eprintf "loss %d, pos (%d, %d), dir %s, straight %d\n%!" loss (fst pos) (snd pos) *)
      (*   (Option.fold ~none:"_" ~some:Dir.to_string dir) *)
      (*   straight; *)
      let queue' = Min_Queue.remove elt queue in
      if pos = dest then (* Printf.eprintf "reached (%d, %d)\n%!" (fst dest) (snd dest); *)
        loss
      else if Hashtbl.mem seen (pos, dir, straight) then loop queue'
      else (
        Hashtbl.replace seen (pos, dir, straight) ();
        (match dir with
        | Some dir ->
            List.concat
              [
                (if straight < max_straight then [ (dir, straight + 1) ] else []);
                (if straight >= min_straight then Dir.[ (turn_left dir, 1); (turn_right dir, 1) ]
                 else []);
              ]
        | None -> Dir.[ (Down, 1); (Right, 1); (Left, 1); (Up, 1) ])
        |> List.filter_map (fun (dir, straight) ->
               let pos' = Grid.Pos.(add pos (of_dir dir)) in
               (* Printf.eprintf "next: pos' (%d, %d), ok %B\n%!" (fst pos') (snd pos') *)
               (*   (Pos.is_valid size pos'); *)
               if Grid.Pos.is_valid size pos' then Some (pos', dir, straight) else None)
        |> List.map (fun (pos', dir, straight) ->
               let loss' = loss + grid.@(pos') in
               (* Printf.eprintf "next: loss' %d, pos' (%d, %d), dir %s, straight %d\n%!" loss' *)
               (*   (fst pos') (snd pos') (Dir.to_string dir) straight; *)
               (loss', pos', Some dir, straight))
        |> List.fold_left (Fun.flip Min_Queue.add) queue'
        |> loop)
    in
    loop (Min_Queue.singleton (0, src, None, 0))

  let of_lines = Grid.of_lines (fun c -> int_of_char c - int_of_char '0')
end
