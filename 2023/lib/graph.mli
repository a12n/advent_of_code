module type VERTEX = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : ?attr:bool -> Format.formatter -> t -> unit
end

module type WEIGHT = sig
  type t

  val zero : t
  val add : t -> t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type vertex
  type weight
  type t

  val make : int -> t
  val add_edge : t -> vertex -> vertex -> weight -> unit
  val replace_edge : t -> vertex -> vertex -> weight -> unit
  val adjacent : t -> vertex -> (vertex * weight) list
  val edges : t -> (vertex * vertex * weight) list
  val vertices : t -> vertex list
  val pp : Format.formatter -> t -> unit
end

module Make_Directed (Vertex : VERTEX) (Weight : WEIGHT) :
  S with type vertex := Vertex.t and type weight := Weight.t

module Make_Undirected (Vertex : VERTEX) (Weight : WEIGHT) :
  S with type vertex := Vertex.t and type weight := Weight.t
