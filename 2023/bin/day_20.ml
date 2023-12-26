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
