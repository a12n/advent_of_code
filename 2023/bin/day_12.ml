open Advent

module Quant = struct
  type t = N_Bad of int | Some_OK | Any_OK
end

module Spring = struct
  type t = Bad | OK

  let of_char = function '.' -> OK | '#' -> Bad | _ -> invalid_arg __FUNCTION__
  let of_char_opt = function '?' -> None | c -> Some (of_char c)
  let to_char = function OK -> '.' | Bad -> '#'
  let opt_to_char = function None -> '?' | Some s -> to_char s
end

module Pattern : sig
  type t = private Spring.t option list * Quant.t list

  val arrangements : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val unfold : t -> t
end = struct
  type t = Spring.t option list * Quant.t list

  let rec arrangements =
    let open Quant in
    let open Spring in
    function
    | [], [] -> 1
    (* Match bad springs. *)
    | elts, N_Bad 0 :: pattern' -> arrangements (elts, pattern')
    | None :: elts', N_Bad n :: pattern' -> arrangements (elts', N_Bad (n - 1) :: pattern')
    | Some Bad :: elts', N_Bad n :: pattern' -> arrangements (elts', N_Bad (n - 1) :: pattern')
    (* Match one good spring. *)
    | Some OK :: elts', Some_OK :: pattern' ->
        arrangements (elts', pattern') + arrangements (elts', Some_OK :: pattern')
    | None :: elts', Some_OK :: pattern' ->
        arrangements (elts', pattern') + arrangements (elts', Some_OK :: pattern')
    (* Match any good springs. *)
    | elts, Any_OK :: pattern' ->
        arrangements (elts, pattern') + arrangements (elts, Some_OK :: pattern')
    (* Other arrangements are impossible. *)
    | _ -> 0

  let of_string s =
    match String.split_on_char ' ' s |> List.map String.trim |> List.filter (( <> ) "") with
    | [ springs; numbers ] ->
        let springs = List.init (String.length springs) (fun i -> Spring.of_char_opt springs.[i]) in
        let num_bad = String.split_on_char ',' numbers |> List.map int_of_string in
        let match_bad = List.map (fun n -> Quant.N_Bad n) num_bad in
        let match_both = List.intersperse Quant.Some_OK match_bad in
        let pattern = (Quant.Any_OK :: match_both) @ [ Quant.Any_OK ] in
        (springs, pattern)
    | _ -> invalid_arg __FUNCTION__

  let to_string (springs, pattern) =
    let springs' = List.to_seq springs |> Seq.map Spring.opt_to_char |> String.of_seq in
    let pattern' =
      List.filter_map (function Quant.N_Bad n -> Some (string_of_int n) | _ -> None) pattern
      |> String.concat ","
    in
    springs' ^ " " ^ pattern'

  let unfold (springs, pattern) =
    let num_copies = 5 in
    let springs' = List.(concat % intersperse [ None ] % make num_copies) springs in
    let pattern' =
      let bad_only = function Quant.N_Bad _ -> true | _ -> false in
      Quant.Any_OK
      :: List.(concat % intersperse [ Quant.Some_OK ] % make num_copies % filter bad_only) pattern
      @ [ Quant.Any_OK ]
    in
    (springs', pattern')
end
