module type VERTEX = sig
  type t

  val compare : t -> t -> int
end

module type WEIGHT = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type vertex
  type weight
  type t

  val empty : t
  val add_edge : t -> vertex -> vertex -> weight -> t
  val replace_edge : t -> vertex -> vertex -> weight -> t
  val remove_edge : t -> vertex -> vertex -> weight -> t
  val adjacent : t -> vertex -> (vertex * weight) list
  val edges : t -> (vertex * vertex * weight) list
  val vertices : t -> vertex list

  val pp :
    ([ `Attr | `Edge ] -> Format.formatter -> vertex -> unit) ->
    (Format.formatter -> weight -> unit) ->
    Format.formatter ->
    t ->
    unit
end

module Make_Directed (Vertex : VERTEX) (Weight : WEIGHT) :
  S with type vertex := Vertex.t and type weight := Weight.t

module Make_Undirected (Vertex : VERTEX) (Weight : WEIGHT) : sig
  include S with type vertex := Vertex.t and type weight := Weight.t

  val components : t -> t list
end
