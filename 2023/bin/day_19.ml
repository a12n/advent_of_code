module Part = struct
  type t = { x : int; m : int; a : int; s : int }
end

module Rule = struct
  type t = Part.t -> (string, bool) result

  let of_string s =
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ "A" ] -> fun _ -> Error true
    | [ "R" ] -> fun _ -> Error false
    | [ cond; act ] ->
        (* TODO *)
        fun _ -> Ok ""
    | _ -> invalid_arg (__FUNCTION__ ^ ": \"" ^ s ^ "\"")
end
