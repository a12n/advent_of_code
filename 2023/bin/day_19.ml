module Part = struct
  type t = { x : int; m : int; a : int; s : int }
  and part = t

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
    type t = part * part

    let full = ({ x = 1; m = 1; a = 1; s = 1 }, { x = 4000; m = 4000; a = 4000; s = 4000 })

    let merge (min1, max1) (min2, max2) =
      ( {
          x = Int.max min1.x min2.x;
          m = Int.max min1.m min2.m;
          a = Int.max min1.a min2.a;
          s = Int.max min1.s min2.s;
        },
        {
          x = Int.min max1.x max2.x;
          m = Int.min max1.m max2.m;
          a = Int.min max1.a max2.a;
          s = Int.min max1.s max2.s;
        } )
  end
end
