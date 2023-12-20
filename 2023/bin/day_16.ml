open Advent

module Dir = struct
  type t = Up | Left | Right | Down

  let add_pos dir (row, col) =
    match dir with
    | Up -> (row - 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)
    | Down -> (row + 1, col)

  let to_string = function Up -> "Up" | Left -> "Left" | Right -> "Right" | Down -> "Down"
end

module Beam : sig
  type t = private int
  (** Beam identifier. *)

  val init : t
  val split : t -> t
end = struct
  type t = int

  let init = 0
  let split = ( + ) 1
end

module Device = struct
  type t = Dir.t * Beam.t -> (Dir.t * Beam.t) list
  (** Beam device transforms a beam travelling in some direction into
      a set of other beams. *)

  (** A no-op beam device. *)
  let pass (to_dir, beam) = [ (to_dir, beam) ]
end

module Mirror = struct
  type t = Upward | Downward

  let reflect m (to_dir, beam) =
    Dir.(
      match (to_dir, m) with
      | Up, Upward -> [ (Right, beam) ]
      | Left, Upward -> [ (Down, beam) ]
      | Right, Upward -> [ (Up, beam) ]
      | Down, Upward -> [ (Left, beam) ]
      | Up, Downward -> [ (Left, beam) ]
      | Left, Downward -> [ (Up, beam) ]
      | Right, Downward -> [ (Down, beam) ]
      | Down, Downward -> [ (Right, beam) ])

  let of_char = function '/' -> Upward | '\\' -> Downward | _ -> invalid_arg __FUNCTION__
  let to_char = function Upward -> '/' | Downward -> '\\'
end

module Splitter = struct
  type t = Horiz | Vert

  let split s (to_dir, beam) =
    Dir.(
      match (to_dir, s) with
      | Left, Horiz | Right, Horiz -> [ (to_dir, beam) ]
      | Up, Vert | Down, Vert -> [ (to_dir, beam) ]
      | Up, Horiz -> [ (Left, beam); (Right, Beam.split beam) ]
      | Down, Horiz -> [ (Right, beam); (Left, Beam.split beam) ]
      | Left, Vert -> [ (Down, beam); (Up, Beam.split beam) ]
      | Right, Vert -> [ (Up, beam); (Down, Beam.split beam) ])

  let of_char = function '-' -> Horiz | '|' -> Vert | _ -> invalid_arg __FUNCTION__
  let to_char = function Horiz -> '-' | Vert -> '|'
end

module Energized_Map = Hashtbl.Make (struct
  type t = int * int

  let equal = Stdlib.( = )
  let hash = Hashtbl.hash
end)

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val size : t -> int * int
  val trace : t -> Dir.t -> int * int -> Dir.t list Energized_Map.t
end = struct
  type t = Device.t array array

  let of_lines =
    let device_of_char = function
      | '.' -> Device.pass
      | c -> (
          try Mirror.(reflect (of_char c)) with Invalid_argument _ -> Splitter.(split (of_char c)))
    in
    Array.of_seq % Seq.map (Array.of_seq % Seq.map device_of_char % String.to_seq)

  let size = Array.matrix_size

  let trace grid start_dir start_pos =
    let n_rows, n_cols = Array.matrix_size grid in
    let energized = Energized_Map.create (n_rows * n_cols) in
    let rec do_trace ((dir, _) as dir_beam) = function
      | row, col when row < 0 || row = n_rows || col < 0 || col = n_cols ->
          (* Beam left the grid. *)
          ()
      | (row, col) as pos ->
          let seen =
            match Energized_Map.find_opt energized pos with Some seen -> seen | None -> []
          in
          if not (List.mem dir seen) then (
            Energized_Map.replace energized pos (dir :: seen);
            List.iter
              (fun ((next_dir, _) as next_dir_beam) ->
                do_trace next_dir_beam Dir.(add_pos next_dir pos))
              (grid.(row).(col) dir_beam))
    in
    do_trace (start_dir, Beam.init) start_pos;
    energized
end
