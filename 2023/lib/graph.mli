module type S = sig
  type ('v, 'w) t

  val make : int -> ('v, 'w) t
  val add_edge : ('v, 'w) t -> 'v -> 'v -> 'w -> unit
  val replace_edge : ('v, 'w) t -> 'v -> 'v -> 'w -> unit
  val adjacent : ('v, 'w) t -> 'v -> ('v * 'w) list
  val edges : ('v, 'w) t -> ('v * 'v * 'w) list
  val vertices : ('v, 'w) t -> 'v list

  val pp :
    vertex:([ `Attr | `Edge ] -> Format.formatter -> 'v -> unit) ->
    weight:(Format.formatter -> 'w -> unit) ->
    Format.formatter ->
    ('v, 'w) t ->
    unit
end

module Directed : S
module Undirected : S
