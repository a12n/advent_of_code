open Advent

module Draw : sig
  type t = { red : int; green : int; blue : int }

  val geq : t -> t -> bool
  val max_elts : t -> t -> t
  val of_string : string -> t
  val power : t -> int
end = struct
  type t = { red : int; green : int; blue : int }

  let zero = { red = 0; green = 0; blue = 0 }
  let geq a b = a.red >= b.red && a.green >= b.green && a.blue >= b.blue

  let max_elts a b =
    {
      red = Int.max a.red b.red;
      green = Int.max a.green b.green;
      blue = Int.max a.blue b.blue;
    }

  let of_string s =
    String.split_on_char ',' s |> List.map String.trim
    |> List.fold_left
         (fun ans s ->
           match String.split_on_char ' ' s with
           | [ n; "red" ] -> { ans with red = int_of_string n }
           | [ n; "green" ] -> { ans with green = int_of_string n }
           | [ n; "blue" ] -> { ans with blue = int_of_string n }
           | _ -> invalid_arg __FUNCTION__)
         zero

  let power { red; green; blue } = red * green * blue
end

module Game : sig
  type t = { id : int; draws : Draw.t list }

  val is_possible : t -> Draw.t -> bool
  val of_string : string -> t
end = struct
  type t = { id : int; draws : Draw.t list }

  let is_possible { draws; _ } total = List.for_all (Draw.geq total) draws

  let of_string s =
    match String.split_on_char ':' s with
    | [ header; list ] ->
        {
          id =
            (match String.split_on_char ' ' header with
            | [ "Game"; id ] -> int_of_string id
            | _ -> invalid_arg __FUNCTION__);
          draws = List.map Draw.of_string (String.split_on_char ';' list);
        }
    | _ -> invalid_arg __FUNCTION__
end
