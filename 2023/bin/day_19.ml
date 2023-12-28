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
        pp_print_string fmt "{x=";
        Segment.pp fmt x;
        pp_print_string fmt "; m=";
        Segment.pp fmt m;
        pp_print_string fmt "; a=";
        Segment.pp fmt a;
        pp_print_string fmt "; s=";
        Segment.pp fmt s;
        pp_print_string fmt "; n=";
        pp_print_int fmt (cardinal r);
        pp_print_string fmt "}")
  end
end
