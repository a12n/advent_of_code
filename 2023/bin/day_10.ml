open Advent

module Pipe = struct
  type t = Start | Up_Down | Right_Left | Up_Right | Up_Left | Down_Left | Down_Right

  let of_char = function
    | 'S' -> Start
    | '|' -> Up_Down
    | '-' -> Right_Left
    | 'L' -> Up_Right
    | 'J' -> Up_Left
    | '7' -> Down_Left
    | 'F' -> Down_Right
    | _ -> invalid_arg __FUNCTION__

  let to_string = function
    | Start -> "S"
    | Up_Down -> "║"
    | Right_Left -> "═"
    | Up_Right -> "╚"
    | Up_Left -> "╝"
    | Down_Left -> "╗"
    | Down_Right -> "╚"

  let has_conn pipe dir =
    match pipe with
    | Start -> true
    | Up_Down -> Dir.(dir = Up || dir = Down)
    | Right_Left -> Dir.(dir = Right || dir = Left)
    | Up_Right -> Dir.(dir = Up || dir = Right)
    | Up_Left -> Dir.(dir = Up || dir = Left)
    | Down_Left -> Dir.(dir = Down || dir = Left)
    | Down_Right -> Dir.(dir = Down || dir = Right)

  let opt_of_char = function '.' -> None | c -> Some (of_char c)
  let opt_to_string = function None -> "." | Some p -> to_string p
end

module Grid : sig
  type t = Pipe.t option array array

  val cycle : t -> (int * int) list option
  val of_lines : string Seq.t -> t
end = struct
  type t = Pipe.t option array array

  let size = Array.matrix_size

  let start pipes =
    Array.(
      find_mapi
        (fun row pipes ->
          find_mapi (fun col pipe -> if pipe = Some Pipe.Start then Some (row, col) else None) pipes)
        pipes)

  let get pipes (row, col) = Array.(value ~default:None (value ~default:[||] pipes row) col)

  let conns pipes pos =
    match get pipes pos with
    | Some pipe ->
        List.filter_map
          (fun dir ->
            let next_pos = Pos.add pos Dir.(to_pos dir) in
            match get pipes next_pos with
            | Some next_pipe ->
                if Pipe.has_conn pipe dir && Pipe.has_conn next_pipe (Dir.neg dir) then
                  Some next_pos
                else None
            | None -> None)
          Dir.[ Up; Right; Down; Left ]
    | None -> []

  (* TODO *)
  let cycles pipes =
    let n, m = size pipes in
    let start = Option.get (start pipes) in
    let visited = Array.make_matrix n m false in
    let rec dfs path ((row, column) as pos) =
      if visited.(row).(column) then if List.last path = pos then [ path ] else []
      else (
        visited.(row).(column) <- true;
        List.concat (List.map (dfs (pos :: path)) (conns pipes pos)))
    in
    dfs [] start

  let cycle pipes =
    match cycles pipes with
    | [] -> None
    | c0 :: cs ->
        Some (List.fold_left (fun max c -> if List.compare_lengths c max > 0 then c else max) c0 cs)

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map Pipe.opt_of_char % String.to_seq)
end
