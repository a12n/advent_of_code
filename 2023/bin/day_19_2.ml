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
        fun parts -> ((action, parts), Part.Range.empty)
    | [ cond; action ] -> (
        let get = Part.Range.get cond.[0] in
        let set = Part.Range.set cond.[0] in
        let value = int_of_string String.(sub cond 2 (length cond - 2)) in
        let action = action_of_string action in
        match cond.[1] with
        | '>' ->
            fun parts ->
              let accept, pass = Segment.partition (value + 1) (get parts) in
              ((action, set parts accept), set parts pass)
        | '<' ->
            fun parts ->
              let accept, pass = Segment.partition value (get parts) in
              ((action, set parts accept), set parts pass)
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

  val eval : t -> Part.Range.t -> (string * Part.Range.t) list * Part.Range.t
  val eval2 : t2 -> Part.Set.t -> (string * Part.Set.t) list * Part.Set.t
  val of_string : string -> t
  val of_string2 : string -> t2
end = struct
  type t = Rule.t list
  type t2 = Rule.t2 list

  let eval wf parts =
    let send, accept, _ =
      List.fold_left
        (fun (send, accept, parts) rule ->
          match rule parts with
          | (`Accept, a), pass -> (send, Part.Range.union a accept, pass)
          | (`Reject, _), pass -> (send, accept, pass)
          | (`Send id, s), pass -> ((id, s) :: send, accept, pass))
        ([], Part.Range.empty, parts) wf
    in
    (List.rev send, accept)

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
    (List.rev send, accept)

  let of_string = List.map Rule.of_string % String.split_on_char ','
  let of_string2 = List.map Rule.of_string2 % String.split_on_char ','
end

module System : sig
  type t
  type t2

  (* Returns accepted part range. *)
  val eval : t -> string -> Part.Range.t -> int
  val eval2 : t2 -> string -> Part.Set.t -> int
  val of_lines : string Seq.t -> t
  val of_lines2 : string Seq.t -> t2
end = struct
  type t = (string, Workflow.t) Hashtbl.t
  type t2 = (string, Workflow.t2) Hashtbl.t

  let rec eval sys id parts =
    let send, accept = Workflow.eval (Hashtbl.find sys id) parts in
    Format.(
      fprintf err_formatter "eval \"%s\": " id;
      Part.Range.pp err_formatter parts;
      pp_print_string err_formatter " ->\n";
      List.iter
        (fun (id, parts) ->
          fprintf err_formatter "- send \"%s\" " id;
          Part.Range.pp err_formatter parts;
          pp_print_newline err_formatter ())
        send;
      pp_print_string err_formatter "- accept ";
      Part.Range.pp err_formatter accept;
      pp_print_newline err_formatter ());
    let accept = Part.Range.cardinal accept in
    let accept =
      List.fold_left (fun accept (id, parts) -> accept + eval sys id parts) accept send
    in
    Format.(
      fprintf err_formatter "accept after \"%s\": " id;
      pp_print_int err_formatter accept;
      pp_print_newline err_formatter ());
    accept

  let rec eval2 sys id parts =
    let send, accept = Workflow.eval2 (Hashtbl.find sys id) parts in
    Format.(
      fprintf err_formatter "eval \"%s\": " id;
      Part.Set.pp err_formatter parts;
      pp_print_string err_formatter " ->\n";
      List.iter
        (fun (id, parts) ->
          fprintf err_formatter "- send \"%s\" " id;
          Part.Set.pp err_formatter parts;
          pp_print_newline err_formatter ())
        send;
      pp_print_string err_formatter "- accept ";
      Part.Set.pp err_formatter accept;
      pp_print_newline err_formatter ());
    let accept = Part.Set.cardinal accept in
    let accept =
      List.fold_left (fun accept (id, parts) -> accept + eval2 sys id parts) accept send
    in
    Format.(
      fprintf err_formatter "accept after \"%s\": " id;
      pp_print_int err_formatter accept;
      pp_print_newline err_formatter ());
    accept

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

  let of_lines2 =
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
  ignore (System.eval, System.of_lines);
  let lines = input_lines stdin in
  let sys = System.of_lines2 lines in
  let accept = System.eval2 sys "in" Part.Set.full in
  print_endline (string_of_int accept)
