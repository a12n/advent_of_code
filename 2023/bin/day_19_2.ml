open Day_19
open Advent

module Rule : sig
  type t = Part.Range.t -> (string * Part.Range.t) * Part.Range.t

  val of_string : string -> t
end = struct
  type t = Part.Range.t -> (string * Part.Range.t) * Part.Range.t

  let of_string s =
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ action ] -> fun parts -> ((action, parts), Part.Range.empty)
    | [ cond; action ] -> (
        let get = Part.Range.get cond.[0] in
        let set = Part.Range.set cond.[0] in
        let value = int_of_string String.(sub cond 2 (length cond - 2)) in
        match cond.[1] with
        | '>' ->
            fun parts ->
              let pass, accept = Segment.partition (value + 1) (get parts) in
              ((action, set parts accept), set parts pass)
        | '<' ->
            fun parts ->
              let accept, pass = Segment.partition value (get parts) in
              ((action, set parts accept), set parts pass)
        | _ -> invalid_arg __FUNCTION__)
    | _ -> invalid_arg __FUNCTION__
end

module Workflow : sig
  type t = Rule.t list

  val eval : t -> Part.Range.t -> (string * Part.Range.t) list
  val of_string : string -> t
end = struct
  type t = Rule.t list

  let eval wf parts =
    let send, _ =
      List.fold_left
        (fun (send, parts) rule ->
          let s, pass = rule parts in
          (s :: send, pass))
        ([], parts) wf
    in
    List.rev send

  let of_string = List.map Rule.of_string % String.split_on_char ','
end

module System : sig
  type t

  val eval : t -> int
  val of_lines : string Seq.t -> t
end = struct
  type t = (string, Workflow.t) Hashtbl.t

  let eval sys =
    let rec loop id parts =
      let send = Workflow.eval (Hashtbl.find sys id) parts in
      Format.(
        fprintf err_formatter "eval \"%s\": " id;
        Part.Range.pp err_formatter parts;
        pp_print_string err_formatter " ->\n";
        List.iter
          (fun (id, parts) ->
            fprintf err_formatter "- send \"%s\" " id;
            Part.Range.pp err_formatter parts;
            pp_print_newline err_formatter ())
          send);
      let accepted =
        List.fold_left
          (fun accepted (id, parts) ->
            accepted
            + match id with "A" -> Part.Range.cardinal parts | "R" -> 0 | _ -> loop id parts)
          0 send
      in
      Format.(
        fprintf err_formatter "accept after \"%s\": " id;
        pp_print_int err_formatter accepted;
        pp_print_newline err_formatter ());
      accepted
    in
    loop "in" Part.Range.full

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
end

let () =
  let lines = input_lines stdin in
  let sys = System.of_lines lines in
  let accepted = System.eval sys in
  print_endline (string_of_int accepted)
