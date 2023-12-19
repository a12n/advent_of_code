open Advent

module Dir = struct
  type t = North | East | South | West

  let neg = function North -> South | East -> West | South -> North | West -> East

  let add_pos dir (row, column) =
    match dir with
    | North -> (row - 1, column)
    | East -> (row, column + 1)
    | South -> (row + 1, column)
    | West -> (row, column - 1)
end

module Pipe = struct
  type t = Start | North_south | East_west | North_east | North_west | South_west | South_east

  let of_char = function
    | 'S' -> Start
    | '|' -> North_south
    | '-' -> East_west
    | 'L' -> North_east
    | 'J' -> North_west
    | '7' -> South_west
    | 'F' -> South_east
    | _ -> invalid_arg __FUNCTION__

  let has_conn pipe dir =
    match pipe with
    | Start -> true
    | North_south -> Dir.(dir = North || dir = South)
    | East_west -> Dir.(dir = East || dir = West)
    | North_east -> Dir.(dir = North || dir = East)
    | North_west -> Dir.(dir = North || dir = West)
    | South_west -> Dir.(dir = South || dir = West)
    | South_east -> Dir.(dir = South || dir = East)

  let of_char_opt = function '.' -> None | c -> Some (of_char c)
end

module Grid : sig
  type t = Pipe.t option array array

  val size : t -> int * int
  val start : t -> (int * int) option
  val get : t -> int * int -> Pipe.t option
  val of_lines : string Seq.t -> t
  val cycles : t -> (int * int) list list
end = struct
  type t = Pipe.t option array array

  let size pipes =
    Array.(match length pipes with 0 -> (0, 0) | n -> (n, length (unsafe_get pipes 0)))

  let start pipes =
    Array.(
      find_mapi
        (fun row pipes ->
          find_mapi (fun col pipe -> if pipe = Some Pipe.Start then Some (row, col) else None) pipes)
        pipes)

  let get pipes (row, col) = Array.(value ~default:None (value ~default:[||] pipes row) col)

  let conns pipes pos =
    List.filter_map
      (fun dir ->
        let*? pipe = get pipes pos in
        let pos' = Dir.add_pos dir pos in
        let*? pipe' = get pipes pos' in
        if Pipe.(has_conn pipe dir && has_conn pipe' (Dir.neg dir)) then Some pos' else None)
      Dir.[ North; East; South; West ]

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

  let of_lines = Array.of_seq % Seq.map (Array.of_seq % Seq.map Pipe.of_char_opt % String.to_seq)
end
