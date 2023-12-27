open Advent

let ( .%{} ) = Hashtbl.find
let ( .%?{} ) = Hashtbl.find_opt
let ( .%{}<- ) = Hashtbl.replace

module Pulse = struct
  type t = Low | High

  let to_string = function Low -> "L" | High -> "H"
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

  let run cfg name pulse =
    let queue = Queue.create () in
    (* Send pulse to input 0 of the module [name]. *)
    Queue.add (pulse, name, 0) queue;
    (* Propagate pulses. *)
    while not (Queue.is_empty queue) do
      let pulse, name, input = Queue.take queue in
      (* Flip-flops. *)
      (match cfg.flip_flops.%?{name} with
      | Some on -> (
          match pulse with
          | Pulse.Low ->
              (* On low pulse, flip state and send to outputs. *)
              cfg.flip_flops.%{name} <- not on;
              let pulse' = if on then Pulse.Low else Pulse.High in
              List.iter
                (fun (dest, input) -> Queue.add (pulse', dest, input) queue)
                cfg.outputs.%{name}
          | Pulse.High ->
              (* On high pulse, do nothing. *)
              ())
      | None ->
          (* Not a flip-flop. *)
          ());
      (* Conjunctions. *)
      (match cfg.conjunctions.%?{name} with
      | Some state ->
          state.(input) <- pulse;
          let pulse' = if Array.for_all (( = ) Pulse.High) state then Pulse.Low else Pulse.High in
          List.iter (fun (dest, input) -> Queue.add (pulse', dest, input) queue) cfg.outputs.%{name}
      | None ->
          (* Not a conjunction. *)
          ());
      (* Broadcasters. *)
      List.iter (fun (dest, input) -> Queue.add (pulse, dest, input) queue) cfg.outputs.%{name}
    done

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
                    match num_inputs.%?{dest} with
                    | Some i ->
                        num_inputs.%{dest} <- i + 1;
                        i
                    | None ->
                        num_inputs.%{dest} <- 1;
                        0
                  in
                  (dest, i))
                Str.(split (regexp "[ ]*,[ ]*") dests)
            in
            outputs.%{mod_name} <- dests;
            match mod_type with
            | '%' -> flip_flops.%{mod_name} <- false
            | '&' -> conjunctions.%{mod_name} <- [||]
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
        outputs.%{name} <- dests)
      outputs;
    (* Allocate conjunctions inputs. *)
    Hashtbl.iter
      (fun name _ -> conjunctions.%{name} <- Array.make num_inputs.%{name} Pulse.Low)
      conjunctions;
    (* Done. *)
    cfg
end
