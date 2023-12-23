module Part = struct
  type t = { x : int; m : int; a : int; s : int }

  let get = function
    | 'x' -> fun { x; _ } -> x
    | 'm' -> fun { m; _ } -> m
    | 'a' -> fun { a; _ } -> a
    | 's' -> fun { s; _ } -> s
    | _ -> invalid_arg __FUNCTION__

  let rating { x; m; a; s } = x + m + a + s
  let of_string s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> { x; m; a; s })

end
