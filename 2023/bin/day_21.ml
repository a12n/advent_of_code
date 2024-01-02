open Advent
open Grid.Ops

module Plot = struct
  type t = Garden | Rock | Start

  let of_char = function
    | '.' -> Garden
    | '#' -> Rock
    | 'S' -> Start
    | _ -> invalid_arg __FUNCTION__

  let to_string = function Garden -> "." | Rock -> "▓" | Start -> "@"
end

module Garden = struct
  type t = Plot.t Grid.t

  let start = Grid.find_pos (( = ) Plot.Start)

  (** Positions reachable from the starting position in [n] steps. *)
  let reachable garden pos n =
    let ((n_rows, n_cols) as size) = Grid.size garden in
    if not (Grid.Pos.is_valid size pos) then invalid_arg __FUNCTION__;
    let state = Array.make_matrix n_rows n_cols [] in
    state.@(pos) <- [ 0 ];
    for i = 0 to n do
      for row = 0 to n_rows - 1 do
        for col = 0 to n_cols - 1 do
          if garden.(row).(col) <> Plot.Rock && List.mem i state.(row).(col) then
            Dir.[ Up; Left; Right; Down ]
            |> List.map (Grid.Pos.add (row, col) % Grid.Pos.of_dir)
            |> List.filter (Grid.Pos.is_valid size)
            |> List.filter (fun pos -> garden.@(pos) <> Plot.Rock)
            |> List.iter (fun pos -> state.@(pos) <- (i + 1) :: state.@(pos))
        done
      done
    done;
    Array.fold_lefti
      (fun result row line ->
        Array.fold_lefti
          (fun result col nums -> if List.mem n nums then (row, col) :: result else result)
          result line)
      [] state

  let num_reachable_row garden (row, col0) n m =
    (*
       is_even n =
       ..#.....#|…|..#.....#|..#.@...#|..#.....#|…|..#.....#
                | | ^ ^ ^ ^ |^ ^ ^ ^ ^| ^ ^ ^ ^ | |

       is_odd n =
       ..#.....#|…|..#.....#|..#.@...#|..#.....#|…|..#.....#
                | |^ ^ ^ ^ ^| ^ ^ ^ ^ |^ ^ ^ ^ ^| |
       *)
    let result =
      Seq.ints (col0 - m) ~until:(col0 + m)
      |> Seq.filter Int.(if is_even n then is_even else is_odd)
      |> Seq.map Array.(get_mod (get_mod garden row))
      |> Seq.filter (( <> ) Plot.Rock)
      |> Seq.length
    in
    Printf.eprintf "pos (%d, %d), n %d, m %d, result %d\n%!" row col0 n m result;
    result

  (** Number of reachable positions in [n] steps from the start position. *)
  let num_reachable garden (row0, col0) n =
    (* TODO:
       Memoization:
       (** Mapping of <grid position, repeat left/right/up/down, is_even> to <number of reachable positions>?. *)
       (Pos.t * [`Left | `Right] option * [`Up | `Down] option * bool, int) Hashtbl.t
    *)
    let upper =
      Seq.ints 0 ~until:n
      |> Seq.map (fun i -> num_reachable_row garden (row0 - i, col0) n (n - i))
      |> Seq.reduce ( + )
    in
    let lower =
      Seq.ints 1 ~until:n
      |> Seq.map (fun i -> num_reachable_row garden (row0 + i, col0) n (n - i))
      |> Seq.reduce ( + )
    in
    upper + lower

  let of_lines = Grid.of_lines (fun _ -> Plot.of_char)

  let pp ?highlight =
    Grid.pp ?highlight (fun fmt plot -> Format.pp_print_string fmt (Plot.to_string plot))
end
