module Line : sig
  type t = (int * int) * (int * int)
  (** Pair of points. *)

  (** {2 Constructors} *)

  val list_of_polygon : (int * int) list -> t list

  (** {2 Predicates} *)

  val is_horiz_aligned : t -> bool
  val is_vert_aligned : t -> bool
  val is_aligned : t -> bool

  (** {2 Area} *)

  val shoelace : t -> int
  val part_area : t -> int

  (** {2 Pretty-printing} *)

  val pp : ?i:int -> Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Joint : sig
  type t = (int * int) * (int * int) * (int * int)
  (** Triple of points. *)

  (** {2 Constructors} *)

  val list_of_polygon : (int * int) list -> t list

  (** {2 Predicates} *)

  val is_horiz_aligned : t -> bool
  val is_vert_aligned : t -> bool
  val is_horiz_forward : t -> bool
  val is_vert_forward : t -> bool
  val is_forward : t -> bool
  val is_horiz_backward : t -> bool
  val is_vert_backward : t -> bool
  val is_backward : t -> bool

  module CCW : sig
    val is_internal_angle : t -> bool
    val is_external_angle : t -> bool
  end

  (** {2 Area} *)

  val part_area : t -> int

  (** {2 Pretty-printing} *)

  val pp : ?i:int -> Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

val compact : (int * int) list -> (int * int) list

val boundary_area : (int * int) list -> int
(** Number of points in the polygon boundary. *)

val interior_area : (int * int) list -> int
(** Gauss's area formula adjusted for integer 2D grids. Polygon is a
    list of points on a grid (the input) connected with implicit lines. *)
