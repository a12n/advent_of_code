open Day_19
open Advent

module Rule : sig
  type t = Part.t -> (string, bool) result option

  val of_string : string -> t
end = struct
  type t = Part.t -> (string, bool) result option

  let of_string s =
    let action_of_string = function "A" -> Error true | "R" -> Error false | name -> Ok name in
    let pred_of_char = function
      | '<' -> Stdlib.( < )
      | '>' -> Stdlib.( > )
      | _ -> invalid_arg __FUNCTION__
    in
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ action ] ->
        let action = action_of_string action in
        fun _ -> Some action
    | [ cond; action ] ->
        let get = Part.get cond.[0] in
        let pred = pred_of_char cond.[1] in
        let value = int_of_string String.(sub cond 2 (length cond - 2)) in
        let action = action_of_string action in
        fun p -> if pred (get p) value then Some action else None
    | _ -> invalid_arg __FUNCTION__
end

module Workflow : sig
  type t = Rule.t list

  val eval : t -> Rule.t
  val of_string : string -> t
end = struct
  type t = Rule.t list

  let eval wf p = List.find_map (fun r -> r p) wf
  let of_string = List.map Rule.of_string % String.split_on_char ','
end

module System : sig
  type t

  val eval : t -> Part.t -> bool
  val of_lines : string Seq.t -> t
end = struct
  type t = (string, Workflow.t) Hashtbl.t

  let eval sys =
    let rec loop id p =
      match Workflow.eval (Hashtbl.find sys id) p with
      | Some (Ok id') -> loop id' p
      | Some (Error rejected) -> rejected
      | _ -> failwith __FUNCTION__
    in
    loop "in"

  let of_lines =
    Seq.fold_left
      (fun sys line ->
        (match String.split_on_char '{' line with
        | [ id; workflow' ] -> (
            match String.split_on_char '}' workflow' with
            | [ workflow; "" ] -> Hashtbl.replace sys id (Workflow.of_string workflow)
            | _ -> invalid_arg __FUNCTION__)
        | _ -> invalid_arg __FUNCTION__);
        sys)
      (Hashtbl.create 100)
    % Seq.take_while (( <> ) "")
end

let () =
  let lines = input_lines stdin in
  let sys = System.of_lines lines in
  Seq.map Part.of_string lines
  |> Seq.filter (System.eval sys)
  |> Seq.map Part.rating |> Seq.reduce ( + ) |> string_of_int |> print_endline
