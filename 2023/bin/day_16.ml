open Advent

module Dir = struct
  type t = Up | Left | Right | Down

  let add_pos dir (row, col) =
    match dir with
    | Up -> (row - 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)
    | Down -> (row + 1, col)
end

module Beam : sig
  type t = private int
  (** Beam identifier. *)

  val compare : t -> t -> int
  val init : t
  val split : t -> t
end = struct
  type t = int

  let compare = Int.compare
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

module Beam_Set = Set.Make (struct
  type t = Dir.t * Beam.t

  let compare = Stdlib.compare
end)

module Energized_Map = Hashtbl.Make (struct
  type t = int * int

  let equal = Stdlib.( = )
  let hash = Hashtbl.hash
end)

module Grid : sig
  type t

  val of_lines : string Seq.t -> t
  val trace : t -> int * int -> Dir.t -> Beam_Set.t Energized_Map.t
end = struct
  type t = [ `Reflect of Mirror.t | `Split of Splitter.t ] option array array

  let opt_of_char = function
    | '.' -> None
    | c -> (
        try Some (`Reflect (Mirror.of_char c))
        with Invalid_argument _ -> Some (`Split (Splitter.of_char c)))

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map opt_of_char % String.to_seq)

  let trace grid pos dir =
    let n_rows, n_cols = Array.matrix_size grid in
    let energized = Energized_Map.create (n_rows * n_cols) in
    let rec do_trace ((row, col) as pos) dir_beam =
      let beams =
        match Energized_Map.find_opt energized pos with
        | Some s ->
            (* The position have already seen some beams. *)
            s
        | None ->
            (* No beams were in the position yet. *)
            Beam_Set.empty
      in
      if not (Beam_Set.mem dir_beam beams) then (
        let beams' = Beam_Set.add dir_beam beams in
        Energized_Map.replace energized pos beams';
        match grid.(fst pos).(snd pos) with
        | None -> do_trace Dir.(add_pos pos dir) (dir, beam)
        | _ ->
            (* TODO *)
            ())
    in
    do_trace pos (dir, Beam.init);
    energized
end
