open Advent

let ( .%{} ) = Hashtbl.find
let ( .%?{} ) = Hashtbl.find_opt
let ( .%{}<- ) = Hashtbl.replace

module Pulse = struct
  type t = Low | High

  let to_string = function Low -> "L" | High -> "H"
end

module Flip_Flop = struct
  type t = bool

  let initial = false

  let eval ff = function
    | Pulse.High -> (ff, [])
    | Pulse.Low when ff -> (false, [ Pulse.Low ])
    | Pulse.Low -> (true, [ Pulse.High ])
end

module Conjunction = struct
  type t = Pulse.t array

  let make n = Array.make n Pulse.Low
end

module Propagator = struct
  type t = (Pulse.t * string * string list) Queue.t

  let button queue =
    Queue.add (Pulse.Low, "button", [ "broadcaster" ]) queue;
    queue
end

module Config = struct
  type t = {
    outputs : (string, (string * int) list) Hashtbl.t;
    flip_flops : (string, bool) Hashtbl.t;
    conjunctions : (string, Pulse.t array) Hashtbl.t;
  }

  let pp fmt { outputs; flip_flops; conjunctions } =
    Format.(
      pp_print_string fmt "outputs:\n";
      Hashtbl.iter
        (fun name dests ->
          List.iter (fun (dest, input) -> fprintf fmt "\t%s -> %s %d\n" name dest input) dests)
        outputs;
      pp_print_string fmt "flip_flops:\n";
      Hashtbl.iter (fun name state -> fprintf fmt "\t%s: %B\n" name state) flip_flops;
      pp_print_string fmt "conjunctions:\n";
      Hashtbl.iter
        (fun name state ->
          fprintf fmt "\t%s:" name;
          Array.iter (fprintf fmt " %s" % Pulse.to_string) state;
          pp_print_newline fmt ())
        conjunctions)

  let make () =
    {
      outputs = Hashtbl.create 100;
      flip_flops = Hashtbl.create 100;
      conjunctions = Hashtbl.create 100;
    }

  let of_lines lines =
    let ({ outputs; flip_flops; conjunctions; _ } as cfg) = make () in
    let num_inputs = Hashtbl.create 100 in
    Seq.iter
      (fun line ->
        match Str.(split (regexp "[ ]*->[ ]*") line) with
        | [ modul; dests ] -> (
            let mod_type, mod_name =
              match Str.string_before modul 1 with
              | "%" -> ('%', Str.string_after modul 1)
              | "&" -> ('&', Str.string_after modul 1)
              | _ -> ('>', modul)
            in
            let dests =
              List.map
                (fun dest ->
                  let i =
                    match Hashtbl.find_opt num_inputs dest with
                    | Some i ->
                        Hashtbl.replace num_inputs dest (i + 1);
                        i
                    | None ->
                        Hashtbl.replace num_inputs dest 1;
                        0
                  in
                  (dest, i))
                Str.(split (regexp "[ ]*,[ ]*") dests)
            in
            Hashtbl.replace outputs mod_name dests;
            match mod_type with
            | '%' -> Hashtbl.replace flip_flops mod_name false
            | '&' -> Hashtbl.replace conjunctions mod_name [||]
            | '>' -> ()
            | _ -> failwith __FUNCTION__)
        | _ -> invalid_arg __FUNCTION__)
      lines;
    (* No additional inputs in flip-flops and broadcasters. *)
    Hashtbl.iter
      (fun name dests ->
        let dests =
          List.map
            (fun (dest, input) ->
              if Hashtbl.mem conjunctions dest then (dest, input) else (dest, 0))
            dests
        in
        Hashtbl.replace outputs name dests)
      outputs;
    (* Allocate conjunctions inputs. *)
    Hashtbl.iter
      (fun name _ ->
        Hashtbl.replace conjunctions name (Array.make (Hashtbl.find num_inputs name) Pulse.Low))
      conjunctions;
    (* Done. *)
    cfg
end
