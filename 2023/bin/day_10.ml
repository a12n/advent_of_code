module Pipe = struct
  type t = Vert | Horiz | North_East | North_West | South_West | South_East

  let of_char = function
    | '|' -> Vert
    | '-' -> Horiz
    | 'L' -> North_East
    | 'J' -> North_West
    | '7' -> South_West
    | 'F' -> South_East
    | _ -> invalid_arg __FUNCTION__

  let of_char_opt = function '.' -> None | c -> Some (of_char c)
end

module Grid : sig
  type t = private { start : int * int; pipes : Pipe.t option array array }

  (* val get : t -> int -> int -> Pipe.t option *)
  val size : t -> int * int
  val start : t -> int * int
  val loops : t -> (int * int) list list
  val of_lines : string Seq.t -> t
end = struct
  type t = { start : int * int; pipes : Pipe.t option array array }

  let size g = Array.(length g.pipes, length g.pipes.(0))
  let start g = g.start

  (* let get g row column = *)
  (*   if row >= 0 && column >= 0 then *)
  (*     let n, m = size g in *)
  (*     if row < n && column < m then g.pipes.(row).(column) else None *)
  (*   else None *)

  let loops g =
    let n, m = size g in
    let visited = Array.make_matrix n m false in
    let rec dfs path ((row, column) as pos) =
      if visited.(row).(column) then
        if (row, column) = g.start then [ pos :: path ] else []
      else (
        visited.(row).(column) <- true;
        (* All possible directions from this position. *)
        (match g.pipes.(row).(column) with
        | None -> []
        | Some Pipe.Vert -> [ (row - 1, column); (row + 1, column) ]
        | Some Pipe.Horiz -> [ (row, column - 1); (row, column + 1) ]
        | Some Pipe.North_East -> [ (row - 1, column); (row, column + 1) ]
        | Some Pipe.North_West -> [ (row - 1, column); (row, column - 1) ]
        | Some Pipe.South_West -> [ (row + 1, column); (row, column - 1) ]
        | Some Pipe.South_East -> [ (row + 1, column); (row, column + 1) ])
        (* Filter invalid directions. *)
        |> List.filter (fun (row, column) ->
               row >= 0 && row < n && column >= 0 && column < m)
        (* Search all valid directions and combine the results. *)
        |> List.map (dfs (pos :: path))
        |> List.concat)
    in
    dfs [] g.start

  let of_lines lines =
    let start = ref (-1, -1) in
    let pipes =
      lines
      |> Seq.mapi (fun row line ->
             String.to_seq line
             |> Seq.mapi (fun column c ->
                    if c = 'S' then (
                      start := (row, column);
                      None)
                    else Pipe.of_char_opt c)
             |> Array.of_seq)
      |> Array.of_seq
    in
    { start = !start; pipes }
end
