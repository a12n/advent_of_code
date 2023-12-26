module Pulse = struct
  type t = Low | High
end

module Flip_Flop = struct
  type t = bool

  let initial = false
end

module Propagator = struct
  type t = (Pulse.t * string * string list) Queue.t

  let button queue =
    Queue.add (Pulse.Low, "button", [ "broadcaster" ]) queue;
    queue
end
