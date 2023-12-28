open Day_19
open Advent

module Rule : sig
  type t = Part.Set.t -> (string * Part.Set.t) * Part.Set.t

  val of_string : string -> t
end = struct
  type t = Part.Set.t -> (string * Part.Set.t) * Part.Set.t

  let of_string s =
    match String.split_on_char ':' s |> List.filter (( <> ) "") with
    | [ action ] -> fun parts -> ((action, parts), Part.Set.empty)
    | [ cond; action ] ->
        let get = Part.Set.get cond.[0] in
        let set = Part.Set.set cond.[0] in
        let value = int_of_string String.(sub cond 2 (length cond - 2)) in
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

  val eval : t -> Part.Set.t -> (string * Part.Set.t) list
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
        Part.Set.pp err_formatter parts;
        pp_print_string err_formatter " ->\n";
        List.iter
          (fun (id, parts) ->
            fprintf err_formatter "- send \"%s\" " id;
            Part.Set.pp err_formatter parts;
            pp_print_newline err_formatter ())
          send;
        pp_print_newline err_formatter ());
      let accepted =
        List.fold_left
          (fun accepted (id, parts) ->
            accepted
            + match id with "A" -> Part.Set.cardinal parts | "R" -> 0 | _ -> loop id parts)
          0 send
      in
      Format.(
        fprintf err_formatter "accept after \"%s\": " id;
        pp_print_int err_formatter accepted;
        pp_print_newline err_formatter ());
      accepted
    in
    loop "in" Part.Set.full

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
