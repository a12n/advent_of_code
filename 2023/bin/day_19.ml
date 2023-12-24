module Part = struct
  type t = { x : int; m : int; a : int; s : int }
  and part = t

  let map2_min p1 p2 =
    { x = Int.min p1.x p2.x; m = Int.min p1.m p2.m; a = Int.min p1.a p2.a; s = Int.min p1.s p2.s }

  let map2_max p1 p2 =
    { x = Int.max p1.x p2.x; m = Int.max p1.m p2.m; a = Int.max p1.a p2.a; s = Int.max p1.s p2.s }

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

    let empty = ({ x = 1; m = 1; a = 1; s = 1 }, { x = 0; m = 0; a = 0; s = 0 })
    let full = ({ x = 1; m = 1; a = 1; s = 1 }, { x = 4000; m = 4000; a = 4000; s = 4000 })

    let inter (min1, max1) (min2, max2) = (map2_max min1 min2, map2_min max1 max2)
  end
end
