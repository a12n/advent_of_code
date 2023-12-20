open Advent

module Grid : sig
  type t

  val min_path : t -> int * int -> int * int -> ((int * int) list * int) option
  val of_lines : string Seq.t -> t
  val size : t -> int * int
end = struct
  type t = int array array

  let of_lines =
    let digit c = int_of_char c - int_of_char '0' in
    Array.of_seq % Seq.map (Array.of_seq % Seq.map digit % String.to_seq)

  let size = Array.matrix_size

  let min_path grid start_pos stop_pos =
    (* TODO *)
    ignore (grid, start_pos, stop_pos);
    None
end
