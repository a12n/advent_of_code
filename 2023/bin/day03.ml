open Advent

module Pos = struct
  type t = int * int
  (** Row and column in the puzzle grid. *)

  let zero = (0, 0)
end

module Region = struct
  type t = { min : Pos.t; max : Pos.t }

  let contains { min = min_row, min_column; max = max_row, max_column }
      (row, column) =
    row >= min_row && row <= max_row && column >= min_column
    && column <= max_column
end

module Part = struct
  type t = { num : int; min : Pos.t; max : Pos.t }

  let is_adjacent { num; min; max } pos =
    let ans = Region.(contains { min; max } pos) in
    Printf.eprintf
      "is_adjacent { num = %d; min = (%d, %d); max = (%d, %d) } (%d, %d) = %B\n\
       %!"
      num (fst min) (snd min) (fst max) (snd max) (fst pos) (snd pos) ans;
    ans

  let start digit (row, column) =
    { num = digit; min = (row - 1, column - 1); max = Pos.zero }

  let update ({ num; _ } as part) digit = { part with num = (num * 10) + digit }
  let finish part pos = { part with max = pos }
end

let is_digit c = c >= '0' && c <= '9'
let int_of_digit c = int_of_char c - int_of_char '0'

module State = struct
  type t = {
    pos : Pos.t;
    current : Part.t option;
    parts : Part.t list;
    symbols : Pos.t list;
  }

  let initial = { pos = Pos.zero; current = None; parts = []; symbols = [] }

  let update ({ pos = (row, column) as pos; current; parts; symbols } as st) =
    function
    | '\n' ->
        (* Finish current part (if any) and go to the next line. *)
        {
          st with
          pos = (row + 1, 0);
          current = None;
          parts =
            (match current with
            | Some p -> Part.finish p (row + 1, column + 1) :: parts
            | None -> parts);
        }
    | ch when is_digit ch ->
        (* Update current part (or start a new one, if there isn't any), go to the next column. *)
        {
          st with
          pos = (row, column + 1);
          current =
            (match current with
            | None -> Some (Part.start (int_of_digit ch) pos)
            | Some p -> Some (Part.update p (int_of_digit ch)));
        }
    | ch ->
        {
          pos = (row, column + 1);
          current = None;
          parts =
            (match current with
            | Some p -> Part.finish p (row + 1, column) :: parts
            | None -> parts);
          symbols = (if ch = '.' then symbols else pos :: symbols);
        }

  let finish = function
    | { current = None; _ } as st -> st
    | { current = Some p; pos = row, column; parts; _ } as st ->
        {
          st with
          current = None;
          parts = Part.finish p (row + 1, column + 1) :: parts;
        }
end

let () =
  let State.{ parts; symbols; _ } =
    input_chars stdin
    |> Seq.fold_left State.update State.initial
    |> State.finish
  in
  let parts =
    List.filter (fun part -> List.exists (Part.is_adjacent part) symbols) parts
  in
  let sum = List.map (fun Part.{num;_} -> num) parts |> List.reduce (+) in
  List.iter
    (fun Part.{ num; min = min_row, min_column; max = max_row, max_column } ->
      Printf.eprintf "part %d from (%d, %d) to (%d, %d)\n" num min_row min_column
        max_row max_column)
    parts;
  List.iter
    (fun (row, column) -> Printf.eprintf "symbol at (%d, %d)\n" row column)
    symbols;
  print_endline (string_of_int sum)
