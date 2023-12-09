open Advent

module Pos = struct
  type t = int * int
  (** Row and column in the puzzle grid. *)

  let to_string (row, column) =
    "(" ^ string_of_int row ^ ", " ^ string_of_int column ^ ")"

  let zero = (0, 0)
end

module Region = struct
  type t = { min : Pos.t; max : Pos.t }

  let contains { min = r_min, c_min; max = r_max, c_max } (r, c) =
    r >= r_min && c >= c_min && r <= r_max && c <= c_max
end

module Part = struct
  type t = { num : int; min : Pos.t; max : Pos.t }

  let is_adjacent { min; max; _ } pos = Region.(contains { min; max } pos)

  let start digit (row, column) =
    { num = digit; min = (row - 1, column - 1); max = Pos.zero }

  let update ({ num; _ } as part) digit = { part with num = (num * 10) + digit }
  let finish part pos = { part with max = pos }

  let to_string { num; min; max } =
    string_of_int num ^ " " ^ Pos.to_string min ^ " " ^ Pos.to_string max
end

module Symbol = struct
  type t = Pos.t

  let to_string pos = "* " ^ Pos.to_string pos
end

let is_digit c = c >= '0' && c <= '9'
let int_of_digit c = int_of_char c - int_of_char '0'

module State = struct
  type t = {
    pos : Pos.t;
    current : Part.t option;
    parts : Part.t list;
    symbols : Symbol.t list;
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

let part1 () =
  let State.{ parts; symbols; _ } =
    input_chars stdin
    |> Seq.fold_left State.update State.initial
    |> State.finish
  in
  (* List.iter prerr_endline (List.map Part.to_string parts); *)
  (* List.iter prerr_endline (List.map Symbol.to_string symbols); *)
  let sum =
    List.filter
      (fun part -> List.exists (Part.is_adjacent part) symbols)
      parts (* All parts adjacent to at least one symbol. *)
    |> List.map (fun Part.{ num; _ } -> num) (* Extract the part numbers. *)
    |> List.reduce ( + ) (* Sum the part numbers. *)
  in
  print_endline (string_of_int sum)

let () = part1 ()
