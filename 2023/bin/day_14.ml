open Advent

module Rock = struct
  type t = Cube | Round

  let of_char = function '#' -> Cube | 'O' -> Round | _ -> invalid_arg __FUNCTION__
  let to_char = function Cube -> '#' | Round -> 'O'
  let load = function Cube -> 0 | Round -> 1
  let opt_load = function Some r -> load r | None -> 0
  let opt_of_char = function '.' -> None | c -> Some (of_char c)
  let opt_to_char = function None -> '.' | Some r -> to_char r
end

module Platform : sig
  type t

  val load : t -> int
  val of_lines : string Seq.t -> t
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
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

  let pp fmt rocks =
    let n_cols, n_rows = Array.matrix_size rocks in
    Format.pp_print_newline fmt ();
    for row = 0 to n_rows - 1 do
      for col = 0 to n_cols - 1 do
        Format.pp_print_char fmt (Rock.opt_to_char rocks.(col).(row))
      done;
      Format.pp_print_newline fmt ()
    done
end
