open Advent

module Quant = struct
  type t = N_Bad of int | Some_OK | Any_OK
end

module Spring = struct
  type t = Bad | OK
end

module Pattern : sig
  type t

  val arrangements : t -> int
  val of_string : string -> t
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
    | None :: elts', Any_OK :: pattern' ->
        arrangements (None :: elts', pattern')
        + arrangements (elts', pattern')
        + arrangements (elts', Some_OK :: pattern')
    | Some Bad :: elts', Any_OK :: pattern' -> arrangements (Some Bad :: elts', pattern')
    | Some OK :: elts', Any_OK :: pattern' ->
        arrangements (elts', pattern') + arrangements (elts', Some_OK :: pattern')
    | [], Any_OK :: pattern' -> arrangements ([], pattern')
    (* Other arrangements are impossible. *)
    | _ -> 0

  let of_string s =
    match String.split_on_char ' ' s |> List.map String.trim |> List.filter (( <> ) "") with
    | [ springs; numbers ] ->
        let springs =
          List.init (String.length springs) (fun i ->
              match springs.[i] with
              | '.' -> Some Spring.OK
              | '#' -> Some Spring.Bad
              | '?' -> None
              | _ -> invalid_arg __FUNCTION__)
        in
        let num_bad = String.split_on_char ',' numbers |> List.map int_of_string in
        let match_bad = List.map (fun n -> Quant.N_Bad n) num_bad in
        let match_both = List.intersperse Quant.Some_OK match_bad in
        let pattern = (Quant.Any_OK :: match_both) @ [ Quant.Any_OK ] in
        (springs, pattern)
    | _ -> invalid_arg __FUNCTION__

  let unfold p =
    (* TODO *)
    p
end
