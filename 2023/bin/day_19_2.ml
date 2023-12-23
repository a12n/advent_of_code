open Day_19
open Advent

module Rule : sig
  (* TODO *)
  type t = Part.Range.t -> unit

  val of_string : string -> t
end = struct
  type t = Part.Range.t -> unit

  let of_string s =
    (* TODO *)
    ignore s;
    failwith __FUNCTION__
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
