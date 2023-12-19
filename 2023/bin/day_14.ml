open Advent

module Rock = struct
  type t = Cube | Round

  let of_char = function '#' -> Cube | 'O' -> Round | _ -> invalid_arg __FUNCTION__
  let load = function Cube -> 0 | Round -> 1
  let opt_load = function Some r -> load r | None -> 0
  let opt_of_char = function '.' -> None | c -> Some (of_char c)
end

module Platform : sig
  type t

  val load : t -> int
  val of_lines : string Seq.t -> t
  val tilt : t -> t
end = struct
  type t = Rock.t option array array

  let load =
    Array.fold_lefti
      (fun total _col rocks ->
        let n_rows = Array.length rocks in
        Array.fold_lefti
          (fun total row rock -> total + ((n_rows - row) * Rock.opt_load rock))
          total rocks)
      0

  let of_lines =
    Array.transpose % Array.of_seq
    % Seq.map (Array.of_seq % Seq.map Rock.opt_of_char % String.to_seq)

  let tilt_inplace rocks =
    (* TODO *)
    ignore rocks

  let tilt rocks =
    let ans = Array.(map copy rocks) in
    tilt_inplace ans;
    ans
end
