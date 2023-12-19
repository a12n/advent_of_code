open Advent

module Rock = struct
  type t = Cube | Round

  let of_char = function '#' -> Cube | 'O' -> Round | _ -> invalid_arg __FUNCTION__
  let opt_of_char = function '.' -> None | c -> Some (of_char c)
end

module Platform : sig
  type t

  val load : t -> int
  val of_lines : string Seq.t -> t
  val tilt : t -> t
end = struct
  type t = Rock.t option array array

  let load _ = 0
  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map Rock.opt_of_char % String.to_seq)
  let tilt = Array.copy
end
