open Advent

let ( .%{} ) = Hashtbl.find
let ( .%%{} ) = Hashtbl.find_opt
let ( .%{}<- ) = Hashtbl.replace

module Pulse = struct
  type t = Low | High

  let to_string = function Low -> "low" | High -> "high"
end

module Stats = struct
  type t = { low : int; high : int }

  let zero = { low = 0; high = 0 }
  let add s1 s2 = { low = s1.low + s2.low; high = s1.high + s2.high }

  let add_pulse s = function
    | Pulse.Low -> { s with low = s.low + 1 }
    | Pulse.High -> { s with high = s.high + 1 }
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

  let push_button cfg =
    let stats = ref Stats.zero in
    let queue = Queue.create () in
    Queue.add (Pulse.Low, "button", ("broadcaster", 0)) queue;
    (* Propagate pulses. *)
    while not (Queue.is_empty queue) do
      let pulse, _src, (name, input) = Queue.take queue in
      stats := Stats.add_pulse !stats pulse;
      (* Flip-flops. *)
      if Hashtbl.mem cfg.flip_flops name then (
        if pulse = Pulse.Low then (
          (* On low pulse, flip state and send to outputs. *)
          let state = cfg.flip_flops.%{name} in
          cfg.flip_flops.%{name} <- not state;
          let pulse' = if state then Pulse.Low else Pulse.High in
          List.iter (fun output -> Queue.add (pulse', name, output) queue) cfg.outputs.%{name}))
      else if Hashtbl.mem cfg.conjunctions name then (
        let state = cfg.conjunctions.%{name} in
        state.(input) <- pulse;
        let pulse' = if Array.for_all (( = ) Pulse.High) state then Pulse.Low else Pulse.High in
        List.iter (fun output -> Queue.add (pulse', name, output) queue) cfg.outputs.%{name})
      else if Hashtbl.mem cfg.outputs name then
        List.iter (fun output -> Queue.add (pulse, name, output) queue) cfg.outputs.%{name}
      else ()
    done;
    !stats

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
                    match num_inputs.%%{dest} with
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
