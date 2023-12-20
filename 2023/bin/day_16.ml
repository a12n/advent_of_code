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

module Mirror = struct
  type t = Upward | Downward

  let reflect m from_dir beam =
    Dir.(
      match (from_dir, m) with
      | Up, Upward -> (Left, beam)
      | Left, Upward -> (Up, beam)
      | Right, Upward -> (Down, beam)
      | Down, Upward -> (Right, beam)
      | Up, Downward -> (Right, beam)
      | Left, Downward -> (Down, beam)
      | Right, Downward -> (Up, beam)
      | Down, Downward -> (Left, beam))

  let of_char = function '/' -> Upward | '\\' -> Downward | _ -> invalid_arg __FUNCTION__
  let to_char = function Upward -> '/' | Downward -> '\\'
end

module Splitter = struct
  type t = Horiz | Vert

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
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val trace : t -> int * int -> Dir.t -> Beam_Set.t Energized_Map.t
end = struct
  type t = [ `Reflect of Mirror.t | `Split of Splitter.t ] option array array

  let opt_of_char = function
    | '.' -> None
    | c -> (
        try Some (`Reflect (Mirror.of_char c))
        with Invalid_argument _ -> Some (`Split (Splitter.of_char c)))

  let opt_to_char = function
    | None -> '.'
    | Some (`Reflect m) -> Mirror.to_char m
    | Some (`Split s) -> Splitter.to_char s

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map opt_of_char % String.to_seq)

  let pp fmt grid =
    let n_rows, n_cols = Array.matrix_size grid in
    Format.pp_print_newline fmt ();
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        Format.pp_print_char fmt (opt_to_char grid.(row).(col))
      done;
      Format.pp_print_newline fmt ()
    done

  let trace grid pos dir =
    let n_rows, n_cols = Array.matrix_size grid in
    let energized = Energized_Map.create (n_rows * n_cols) in
    let rec do_trace beam pos dir =
      let next_pos = Dir.add_pos dir pos in
      let beams =
        match Energized_Map.find_opt energized next_pos with
        | Some s ->
            (* Next position have already seen some beams. *)
            s
        | None ->
            (* No beams were in next position yet. *)
            Beam_Set.empty
      in
      if not (Beam_Set.mem (dir, beam) beams) then (
        let beams' = Beam_Set.add (dir, beam) beams in
        Energized_Map.replace energized next_pos beams';
        match grid.(fst next_pos).(snd next_pos) with
        | None -> do_trace beam next_pos dir
        | _ ->
            (* TODO *)
            ())
    in
    do_trace Beam.init pos dir;
    energized
end
