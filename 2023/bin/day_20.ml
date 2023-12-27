module Pulse = struct
  type t = Low | High
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
  type t = unit

  let make () = ()

  let of_lines =
    (* String right of "->" creates numbered and named connection nodes. E.g.,
       broadcaster -> a, b, c

       Creates nodes a-0, b-0, c-0.
    *)
    Seq.fold_left (fun cfg line ->
        (* TODO *)
        ignore line;
        cfg
      ) (make ())
end
