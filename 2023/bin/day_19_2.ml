open Day_19
open Advent

module Rule : sig
  type t = Part.Range.t -> ([ `Accept | `Reject | `Send of string ] * Part.Range.t) * Part.Range.t
  type t2 = Part.Set.t -> ([ `Accept | `Reject | `Send of string ] * Part.Set.t) * Part.Set.t

  val of_string : string -> t
  val of_string2 : string -> t2
end = struct
  type t = Part.Range.t -> ([ `Accept | `Reject | `Send of string ] * Part.Range.t) * Part.Range.t
  type t2 = Part.Set.t -> ([ `Accept | `Reject | `Send of string ] * Part.Set.t) * Part.Set.t

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

  let of_string2 s =
    let action_of_string = function "A" -> `Accept | "R" -> `Reject | name -> `Send name in
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ action ] ->
        let action = action_of_string action in
        fun parts -> ((action, parts), Part.Set.empty)
    | [ cond; action ] ->
        let get = Part.Set.get cond.[0] in
        let set = Part.Set.set cond.[0] in
        let value = int_of_string String.(sub cond 2 (length cond - 2)) in
        let action = action_of_string action in
        let pred =
          match cond.[1] with
          | '>' -> fun n -> n > value
          | '<' -> fun n -> n < value
          | _ -> invalid_arg __FUNCTION__
        in
        fun parts ->
          let send, pass = Part.Set.Int_Set.partition pred (get parts) in
          ((action, set parts send), set parts pass)
    | _ -> invalid_arg __FUNCTION__
end

module Workflow : sig
  type t = Rule.t list
  type t2 = Rule.t2 list

  val eval : t -> Part.Range.t -> (string * Part.Range.t) list * Part.Range.t list
  val eval2 : t2 -> Part.Set.t -> (string * Part.Set.t) list * Part.Set.t
  val of_string : string -> t
  val of_string2 : string -> t2
end = struct
  type t = Rule.t list
  type t2 = Rule.t2 list

  let eval wf p =
    let sent, accepted, _ =
      List.fold_left
        (fun (sent, accepted, p) r ->
          match r p with
          | (`Accept, accept), pass -> (sent, accept :: accepted, pass)
          | (`Reject, _), pass -> (sent, accepted, pass)
          | (`Send id, send), pass -> ((id, send) :: sent, accepted, pass))
        ([], [], p) wf
    in
    (sent, accepted)

  let eval2 wf parts =
    let send, accept, _ =
      List.fold_left
        (fun (send, accept, parts) rule ->
          match rule parts with
          | (`Accept, a), pass -> (send, Part.Set.union accept a, pass)
          | (`Reject, _), pass -> (send, accept, pass)
          | (`Send id, s), pass -> ((id, s) :: send, accept, pass))
        ([], Part.Set.empty, parts) wf
    in
    (send, accept)

  let of_string = List.map Rule.of_string % String.split_on_char ','
  let of_string2 = List.map Rule.of_string2 % String.split_on_char ','
end

module System : sig
  type t

  (* Returns accepted part range. *)
  val eval : t -> Part.Set.t
  val of_lines : string Seq.t -> t
end = struct
  type t = (string, Workflow.t2) Hashtbl.t

  let eval sys =
    ignore Workflow.eval;
    let rec loop id parts =
      let send, accept = Workflow.eval2 (Hashtbl.find sys id) parts in
      Format.(
        fprintf err_formatter "eval \"%s\" " id;
        Part.Set.pp err_formatter parts;
        pp_print_newline err_formatter ()
      );
      List.fold_left (fun accept (id, parts) ->
          Part.Set.union accept (loop id parts)) accept send in
    loop "in" Part.Set.full

  let of_lines =
    ignore Workflow.of_string;
    Seq.fold_left
      (fun sys line ->
        (match String.split_on_char '{' line with
        | [ id; workflow' ] -> (
            match String.split_on_char '}' workflow' with
            | [ workflow; "" ] -> Hashtbl.replace sys id (Workflow.of_string2 workflow)
            | _ -> invalid_arg __FUNCTION__)
        | _ -> invalid_arg __FUNCTION__);
        sys)
      (Hashtbl.create 100)
    % Seq.take_while (( <> ) "")
end

let () =
  let lines = input_lines stdin in
  let sys = System.of_lines lines in
  let accept = System.eval sys in
  print_endline (string_of_int (Part.Set.cardinal accept ))
