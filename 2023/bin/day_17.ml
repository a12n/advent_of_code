open Advent

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val path : t -> Pos.t -> Pos.t -> (int * Pos.t list) option
  val pp : ?path:Pos.t list -> Format.formatter -> t -> unit
  val size : t -> int * int
end = struct
  type t = int array array

  let size = Array.matrix_size
  let ( .@[] ) a (row, col) = a.(row).(col)
  let ( .@[]<- ) a (row, col) value = a.(row).(col) <- value

  let path grid src dest =
    let ((n_rows, n_cols) as size) = Array.matrix_size grid in
    if not Pos.(is_valid size src && is_valid size dest) then
      invalid_arg (__FUNCTION__ ^ ": invalid src or dest");
    let dist = Array.make_matrix n_rows n_cols max_int in
    let prev = Array.make_matrix n_rows n_cols None in
    let rec straight_line pos dir n =
      if n > 0 then
        match prev.@[pos] with
        | None -> false
        | Some dir' when dir' <> dir -> false
        | Some _ -> straight_line (Pos.add pos dir) dir (n - 1)
      else true
    in
    let queue = Queue.create () in
    dist.@[src] <- 0;
    Queue.add src queue;
    let rec loop () =
      match Queue.take_opt queue with
      | None -> ()
      | Some cur when cur = dest -> ()
      | Some cur ->
          Printf.eprintf "cur (%d, %d)\n%!" (fst cur) (snd cur);
          List.map (Pos.add cur) [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
          |> List.filter (Pos.is_valid size)
          |> List.iter (fun next ->
                 let alt = dist.@[cur] + grid.@[next] in
                 let dir = Pos.sub cur next in
                 Printf.printf "next (%d, %d), dist %d, alt %d\n%!" (fst next) (snd next)
                   dist.@[next] alt;
                 if alt < dist.@[next] && not (straight_line cur dir 2) then (
                   dist.@[next] <- alt;
                   prev.@[next] <- Some dir;
                   Queue.add next queue));
          loop ()
    in
    loop ();
    if dist.@[dest] <> max_int then
      let rec backtrack path cur =
        match prev.@[cur] with
        | None -> path
        | Some dir -> backtrack (cur :: path) Pos.(add cur dir)
      in
      Some (dist.@[dest], backtrack [] dest)
    else None

  let of_lines =
    let digit c = int_of_char c - int_of_char '0' in
    Array.of_seq % Seq.map (Array.of_seq % Seq.map digit % String.to_seq)

  let pp ?(path = []) fmt grid =
    Array.iteri
      (fun row line ->
        Array.iteri
          (fun col heat ->
            if List.mem (row, col) path then (
              Format.(
                pp_print_string fmt "\x1b[32m";
                pp_print_int fmt heat;
                pp_print_string fmt "\x1b[0m"))
            else Format.(pp_print_int fmt heat))
          line;
        Format.pp_print_newline fmt ())
      grid
end
