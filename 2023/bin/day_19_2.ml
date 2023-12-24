open Day_19
open Advent

module Rule : sig
  type t = Part.Range.t -> ([ `Accept | `Reject | `Send of string ] * Part.Range.t) * Part.Range.t

  val of_string : string -> t
end = struct
  type t = Part.Range.t -> ([ `Accept | `Reject | `Send of string ] * Part.Range.t) * Part.Range.t

  let of_string s =
    let action_of_string = function "A" -> `Accept | "R" -> `Reject | name -> `Send name in
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ action ] ->
        let action = action_of_string action in
        fun p -> ((action, p), Part.Range.empty)
    | [ cond; action ] -> (
        let get = Part.get cond.[0] in
        let set = Part.set cond.[0] in
        let value = int_of_string String.(sub cond 2 (length cond - 2)) in
        let action = action_of_string action in
        match cond.[1] with
        | '>' ->
            fun (min, max) ->
              let min' = set min (Int.max (get min) (value + 1)) in
              let max' = set max (Int.min (get max) value) in
              ((action, (min', max)), (min, max'))
        | '<' ->
            fun (min, max) ->
              let max' = set max (Int.min (get max) (value - 1)) in
              let min' = set min (Int.max (get min) value) in
              ((action, (min, max')), (min', max))
        | _ -> invalid_arg __FUNCTION__)
    | _ -> invalid_arg __FUNCTION__
end

module Workflow : sig
  type t = Rule.t list

  val eval : t -> Part.Range.t -> (string * Part.Range.t) list * Part.Range.t * Part.Range.t
  val of_string : string -> t
end = struct
  type t = Rule.t list

  let eval wf p =
    (* TODO *)
    ignore wf;
    ([], p, p)

  let of_string s =
    (* TODO *)
    ignore s;
    []
end

module System : sig
  type t

  val eval : t -> Part.Range.t * Part.Range.t
  val of_lines : string Seq.t -> t
end = struct
  type t = (string, Workflow.t) Hashtbl.t

  let eval sys =
    (* TODO *)
    ignore sys;
    (Part.Range.full, Part.Range.full)

  let of_lines lines =
    (* TODO *)
    ignore lines;
    Hashtbl.create 100
end

let () =
  let lines = input_lines stdin in
  let sys = System.of_lines lines in
  (* TODO *)
  ignore sys
