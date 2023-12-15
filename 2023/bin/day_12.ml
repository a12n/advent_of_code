module Spring_row : sig
  type t = private bool option list * int list

  val arrangements : t -> int
  val of_string : string -> t
end = struct
  type t = bool option list * int list

  let rec arrangements = function
    | [], [] -> 1
    | Some true :: conds, 0 :: numbers -> arrangements (conds, numbers)
    | Some true :: conds, numbers -> arrangements (conds, numbers)
    | Some false :: _, 0 :: _ -> 0
    | Some false :: conds, n :: numbers -> arrangements (conds, (n - 1) :: numbers)
    (* TODO *)
    | _ -> 0

  let of_string s =
    match String.split_on_char ' ' s |> List.map String.trim |> List.filter (( <> ) "") with
    | [ springs; numbers ] ->
        let springs =
          List.init (String.length springs) (fun i ->
              match springs.[i] with
              | '.' -> Some true
              | '#' -> Some false
              | '?' -> None
              | _ -> invalid_arg __FUNCTION__)
        in
        let numbers = String.split_on_char ',' numbers |> List.map int_of_string in
        (springs, numbers)
    | _ -> invalid_arg __FUNCTION__
end
