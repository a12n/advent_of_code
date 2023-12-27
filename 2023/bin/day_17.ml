open Advent
open Grid.Ops
module Pos = Grid.Pos

module Heat_Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val path : t -> Pos.t -> Pos.t -> (int * Pos.t list) option
  val heat_loss : ?min_straight:int -> ?max_straight:int -> t -> Pos.t -> Pos.t -> int
  val pp : ?path:Pos.t list -> Format.formatter -> t -> unit
  val size : t -> int * int
end = struct
  type t = int Grid.t

  let cell_color path pos = if List.mem pos path then ("\x1b[32m", "\x1b[0m") else ("", "")
  let pp ?(path = []) = Grid.pp ~highlight:path ~sgr:"\x1b[32m" Format.pp_print_int

  let pp_dist ?(path = []) fmt grid dist prev =
    let n_rows, n_cols = Array.matrix_size grid in
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        let dir =
          match prev.(row).(col) with
          | Some (0, 0) | None -> "."
          | Some v -> Dir.(to_string (of_pos v))
        in
        let color_on, color_off = cell_color path (row, col) in
        Format.fprintf fmt " %s[%s %1d %3d]%s" color_on dir
          grid.(row).(col)
          dist.(row).(col)
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
               let pos' = Grid.Pos.add pos (Dir.to_pos dir) in
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

  let path grid src dest =
    let ((n_rows, n_cols) as size) = Array.matrix_size grid in
    if not Pos.(is_valid size src && is_valid size dest) then
      invalid_arg (__FUNCTION__ ^ ": invalid src or dest");
    let dist = Array.make_matrix n_rows n_cols max_int in
    let prev = Array.make_matrix n_rows n_cols None in
    let rec straight_line pos dir n =
      if n > 1 then
        match prev.@(pos) with
        | None -> false
        | Some dir' when dir' <> dir -> false
        | Some _ -> straight_line (Pos.add pos dir) dir (n - 1)
      else true
    in
    dist.@(src) <- 0;
    let rec loop queue =
      match Priority_Queue.min_elt_opt queue with
      | None -> ()
      | Some ((_, cur) as cur_elt) ->
          let queue = Priority_Queue.remove cur_elt queue in
          Printf.eprintf "cur (%d, %d)\n%!" (fst cur) (snd cur);
          List.map (Pos.add cur) [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
          |> List.filter (Pos.is_valid size)
          |> List.fold_left (update_next cur) queue
          |> loop
    and update_next cur queue next =
      let alt = dist.@(cur) + grid.@(next) in
      let dir = Pos.sub cur next in
      Printf.eprintf "next (%d, %d), dist %d, alt %d\n%!" (fst next) (snd next) dist.@(next) alt;
      if alt < dist.@(next) && not (straight_line cur dir 4) then (
        dist.@(next) <- alt;
        prev.@(next) <- Some dir;
        Priority_Queue.add (alt, next) queue)
      else queue
    in
    loop (Priority_Queue.singleton (0, src));
    if dist.@(dest) <> max_int then (
      let rec backtrack path cur =
        match prev.@(cur) with
        | None -> path
        | Some dir -> backtrack (cur :: path) Pos.(add cur dir)
      in
      let path = src :: backtrack [] dest in
      pp_dist ~path Format.err_formatter grid dist prev;
      Some (dist.@(dest), path))
    else None

  let of_lines = Grid.of_lines (fun c -> int_of_char c - int_of_char '0')
end
