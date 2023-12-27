open Advent

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val path : t -> Pos.t -> Pos.t -> (int * Pos.t list) option
  val heat_loss : t -> Pos.t -> Pos.t -> int
  val pp : ?path:Pos.t list -> Format.formatter -> t -> unit
  val size : t -> int * int
end = struct
  type t = int array array

  let cell_color path pos = if List.mem pos path then ("\x1b[32m", "\x1b[0m") else ("", "")

  let pp ?(path = []) fmt grid =
    Array.iteri
      (fun row line ->
        Array.iteri
          (fun col heat ->
            let color_on, color_off = cell_color path (row, col) in
            Format.(
              pp_print_string fmt color_on;
              pp_print_int fmt heat;
              pp_print_string fmt color_off))
          line;
        Format.pp_print_newline fmt ())
      grid

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

  let size = Array.matrix_size
  let ( .@[] ) a (row, col) = a.(row).(col)
  let ( .@[]<- ) a (row, col) value = a.(row).(col) <- value

  module Priority_Queue = Set.Make (struct
    type t = int * (int * int)

    let compare = Stdlib.compare
  end)

  module Min_Queue = Set.Make (struct
    type t = int * Pos.t * Dir.t option * int

    let compare = Stdlib.compare
  end)

  let heat_loss grid src dest =
    let size = Array.matrix_size grid in
    if not Pos.(is_valid size src && is_valid size dest) then invalid_arg __FUNCTION__;
    let rec loop queue =
      let ((loss, pos, dir, straight) as elt) = Min_Queue.min_elt queue in
      let queue' = Min_Queue.remove elt queue in
      if pos = dest then loss
      else
        (match dir with
        | Some dir -> Dir.[ (dir, straight + 1); (turn_left dir, 1); (turn_right dir, 1) ]
        | None -> Dir.[ (Down, 1); (Right, 1); (Left, 1); (Up, 1) ])
        |> List.filter (fun (_, straight) -> straight < 3)
        |> List.filter_map (fun (dir, straight) ->
               let pos' = Pos.add pos (Dir.to_pos dir) in
               if Pos.is_valid size pos' then Some (pos', dir, straight) else None)
        |> List.map (fun (pos', dir, straight) ->
               let loss' = loss + grid.@[pos'] in
               (loss', pos', Some dir, straight))
        |> List.fold_left (Fun.flip Min_Queue.add) queue'
        |> loop
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
        match prev.@[pos] with
        | None -> false
        | Some dir' when dir' <> dir -> false
        | Some _ -> straight_line (Pos.add pos dir) dir (n - 1)
      else true
    in
    dist.@[src] <- 0;
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
      let alt = dist.@[cur] + grid.@[next] in
      let dir = Pos.sub cur next in
      Printf.eprintf "next (%d, %d), dist %d, alt %d\n%!" (fst next) (snd next) dist.@[next] alt;
      if alt < dist.@[next] && not (straight_line cur dir 4) then (
        dist.@[next] <- alt;
        prev.@[next] <- Some dir;
        Priority_Queue.add (alt, next) queue)
      else queue
    in
    loop (Priority_Queue.singleton (0, src));
    if dist.@[dest] <> max_int then (
      let rec backtrack path cur =
        match prev.@[cur] with
        | None -> path
        | Some dir -> backtrack (cur :: path) Pos.(add cur dir)
      in
      let path = src :: backtrack [] dest in
      pp_dist ~path Format.err_formatter grid dist prev;
      Some (dist.@[dest], path))
    else None

  let of_lines =
    let digit c = int_of_char c - int_of_char '0' in
    Array.of_seq % Seq.map (Array.of_seq % Seq.map digit % String.to_seq)
end
