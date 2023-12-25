open Advent

module Quant = struct
  type t = N_Bad of int | Some_OK | Any_OK
end

module Pattern : sig
  type t = private string * Quant.t list

  val arrangements : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val unfold : t -> t
end = struct
  type t = string * Quant.t list

  let arrangements (springs, pattern) =
    let open Quant in
    let n = String.length springs in
    let rec loop i pattern =
      if i = n then match pattern with [] -> 1 | [ Any_OK ] -> 1 | _ -> 0
      else
        match (springs.[i], pattern) with
        (* Match bad springs. *)
        | '?', N_Bad n :: pattern' ->
            loop (i + 1) (if n > 1 then N_Bad (n - 1) :: pattern' else pattern')
        | '#', N_Bad n :: pattern' ->
            loop (i + 1) (if n > 1 then N_Bad (n - 1) :: pattern' else pattern')
        (* Match one good spring. *)
        | '.', Some_OK :: pattern' -> loop (i + 1) pattern' + loop (i + 1) (Some_OK :: pattern')
        | '?', Some_OK :: pattern' -> loop (i + 1) pattern' + loop (i + 1) (Some_OK :: pattern')
        (* Match any good springs. *)
        | _, Any_OK :: pattern' -> loop i pattern' + loop i (Some_OK :: pattern')
        (* Other arrangements are impossible. *)
        | _ -> 0
    in
    loop 0 pattern

  let of_string s =
    match String.split_on_char ' ' s |> List.map String.trim |> List.filter (( <> ) "") with
    | [ springs; numbers ] ->
        let springs =
          String.map
            (function '?' -> '?' | '.' -> '.' | '#' -> '#' | _ -> invalid_arg __FUNCTION__)
            springs
        in
        let num_bad = String.split_on_char ',' numbers |> List.map int_of_string in
        let match_bad = List.map (fun n -> Quant.N_Bad n) num_bad in
        let match_both = List.intersperse Quant.Some_OK match_bad in
        let pattern = (Quant.Any_OK :: match_both) @ [ Quant.Any_OK ] in
        (springs, pattern)
    | _ -> invalid_arg __FUNCTION__

  let to_string (springs, pattern) =
    let pattern' =
      List.filter_map (function Quant.N_Bad n -> Some (string_of_int n) | _ -> None) pattern
      |> String.concat ","
    in
    springs ^ " " ^ pattern'

  let unfold (springs, pattern) =
    let num_copies = 5 in
    let springs' = String.concat "?" (List.init num_copies (fun _ -> springs)) in
    let pattern' =
      let bad_only = function Quant.N_Bad _ -> true | _ -> false in
      Quant.Any_OK
      :: List.(intersperse Quant.Some_OK % concat % make num_copies % filter bad_only) pattern
      @ [ Quant.Any_OK ]
    in
    (springs', pattern')
end
