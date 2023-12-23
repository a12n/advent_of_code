open Advent

module Part = struct
  type t = { x : int; m : int; a : int; s : int }

  let get = function
    | 'x' -> fun { x; _ } -> x
    | 'm' -> fun { m; _ } -> m
    | 'a' -> fun { a; _ } -> a
    | 's' -> fun { s; _ } -> s
    | _ -> invalid_arg __FUNCTION__

  let rating { x; m; a; s } = x + m + a + s
  let of_string s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> { x; m; a; s })
end

module Rule = struct
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

module Workflow = struct
  type t = Rule.t list

  let eval wf p = List.find_map (fun r -> r p) wf
  let of_string = List.map Rule.of_string % String.split_on_char ','
end

module System = struct
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
