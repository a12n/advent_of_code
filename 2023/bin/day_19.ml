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
