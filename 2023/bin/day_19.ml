module Part = struct
  type t = { x : int; m : int; a : int; s : int }

  let get = function
    | "x" -> fun { x; _ } -> x
    | "m" -> fun { m; _ } -> m
    | "a" -> fun { a; _ } -> a
    | "s" -> fun { s; _ } -> s
    | _ -> invalid_arg __FUNCTION__
end

module Rule = struct
  type t = Part.t -> (string, bool) result option

  let of_string s =
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ "A" ] -> fun _ -> Some (Error true)
    | [ "R" ] -> fun _ -> Some (Error false)
    | [ action ] -> fun _ -> Some (Ok action)
    | [ cond; action ] -> (
        let pred, prop, value =
          match String.split_on_char '<' cond with
          | [ prop; value ] -> (Stdlib.( < ), prop, value)
          | [ cond ] -> (
              match String.split_on_char '>' cond with
              | [ prop; value ] -> (Stdlib.( > ), prop, value)
              | _ -> invalid_arg (__FUNCTION__ ^ ": \"" ^ cond ^ "\""))
          | _ -> invalid_arg __FUNCTION__
        in
        let prop =
          match prop with
          | "x" -> fun Part.{ x; _ } -> x
          | "m" -> fun Part.{ m; _ } -> m
          | "a" -> fun Part.{ a; _ } -> a
          | "s" -> fun Part.{ s; _ } -> s
          | _ -> invalid_arg __FUNCTION__
        in
        let value = int_of_string value in
        let pred p = pred (prop p) value in
        match action with
        | "A" -> fun p -> if pred p then Some (Error true) else None
        | "R" -> fun p -> if pred p then Some (Error false) else None
        | name -> fun p -> if pred p then Some (Ok name) else None)
    | _ -> invalid_arg __FUNCTION__
end
