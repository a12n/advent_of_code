open Advent

module Part = struct
  type t = { x : int; m : int; a : int; s : int }

  let get = function
    | 'x' -> fun { x; _ } -> x
    | 'm' -> fun { m; _ } -> m
    | 'a' -> fun { a; _ } -> a
    | 's' -> fun { s; _ } -> s
    | _ -> invalid_arg __FUNCTION__

  let set = function
    | 'x' -> fun p x -> { p with x }
    | 'm' -> fun p m -> { p with m }
    | 'a' -> fun p a -> { p with a }
    | 's' -> fun p s -> { p with s }
    | _ -> invalid_arg __FUNCTION__

  let rating { x; m; a; s } = x + m + a + s
  let of_string s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> { x; m; a; s })

  module Set = struct
    module Int_Set = Set.Make (Int)

    type t = { x : Int_Set.t; m : Int_Set.t; a : Int_Set.t; s : Int_Set.t }

    let get = function
      | 'x' -> fun { x; _ } -> x
      | 'm' -> fun { m; _ } -> m
      | 'a' -> fun { a; _ } -> a
      | 's' -> fun { s; _ } -> s
      | _ -> invalid_arg __FUNCTION__

    let set = function
      | 'x' -> fun p x -> { p with x }
      | 'm' -> fun p m -> { p with m }
      | 'a' -> fun p a -> { p with a }
      | 's' -> fun p s -> { p with s }
      | _ -> invalid_arg __FUNCTION__

    let empty = { x = Int_Set.empty; m = Int_Set.empty; a = Int_Set.empty; s = Int_Set.empty }

    let full =
      let x = Int_Set.of_seq Seq.(take 4000 (ints 1)) in
      { x; m = x; a = x; s = x }

    let union s1 s2 =
      {
        x = Int_Set.union s1.x s2.x;
        m = Int_Set.union s1.m s2.m;
        a = Int_Set.union s1.a s2.a;
        s = Int_Set.union s1.s s2.s;
      }

    let cardinal { x; m; a; s } = Int_Set.(cardinal x * cardinal m * cardinal a * cardinal s)

    let pp_set fmt set =
      Format.pp_print_char fmt '{';
      let prev =
        Int_Set.fold
          (fun elt prev ->
            match (prev, elt) with
            | 0, elt ->
                Format.(
                  pp_print_char fmt ' ';
                  pp_print_int fmt elt);
                elt
            | prev, elt when elt = prev + 1 -> elt
            | _, elt ->
                Format.(
                  pp_print_string fmt "…";
                  pp_print_int fmt elt);
                0)
          set 0
      in
      if prev <> 0 then (
        Format.(
          pp_print_string fmt "…";
          pp_print_int fmt prev));
      Format.pp_print_string fmt " }"

    let pp fmt { x; m; a; s } =
      Format.(
        pp_print_string fmt "{";
        pp_set fmt x;
        pp_print_string fmt "; ";
        pp_set fmt m;
        pp_print_string fmt "; ";
        pp_set fmt a;
        pp_print_string fmt "; ";
        pp_set fmt s;
        pp_print_string fmt "} ";
        pp_print_int fmt (cardinal { x; m; a; s }))
  end

  module Range = struct
    type t = { x : Segment.t; m : Segment.t; a : Segment.t; s : Segment.t }

    let get = function
      | 'x' -> fun { x; _ } -> x
      | 'm' -> fun { m; _ } -> m
      | 'a' -> fun { a; _ } -> a
      | 's' -> fun { s; _ } -> s
      | _ -> invalid_arg __FUNCTION__

    let set = function
      | 'x' -> fun p x -> { p with x }
      | 'm' -> fun p m -> { p with m }
      | 'a' -> fun p a -> { p with a }
      | 's' -> fun p s -> { p with s }
      | _ -> invalid_arg __FUNCTION__

    let empty =
      let s = Segment.empty in
      { x = s; m = s; a = s; s }

    let full =
      let s = Segment.of_endpoints 1 4000 in
      { x = s; m = s; a = s; s }

    let union p1 p2 =
      {
        x = Segment.union p1.x p2.x;
        m = Segment.union p1.m p2.m;
        a = Segment.union p1.a p2.a;
        s = Segment.union p1.s p2.s;
      }

    let cardinal { x; m; a; s } = Segment.(length x * length m * length a * length s)

    let pp fmt ({ x; m; a; s } as r) =
      Format.(
        pp_print_string fmt "|x ";
        Segment.pp fmt x;
        pp_print_string fmt " m ";
        Segment.pp fmt m;
        pp_print_string fmt " a ";
        Segment.pp fmt a;
        pp_print_string fmt " s ";
        Segment.pp fmt s;
        pp_print_string fmt "| = ";
        pp_print_int fmt (cardinal r))
  end
end
