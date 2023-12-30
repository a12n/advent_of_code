include Stdlib.Fun

module Ops = struct
  let ( % ) f g x = f (g x)
end
