let update_hash h c = 17 * (h + int_of_char c) mod 256
let hash = String.fold_left update_hash 0

module Step = struct
  type t = { label : string; op : [ `Insert of int | `Remove ] }

  let of_string s =
    let digit c =
      if c >= '0' && c <= '9' then int_of_char c - int_of_char '0' else invalid_arg __FUNCTION__
    in
    let n = String.length s in
    if n < 2 then invalid_arg __FUNCTION__;
    if s.[n - 1] = '-' then { label = String.sub s 0 (n - 1); op = `Remove }
    else if s.[n - 2] = '=' then { label = String.sub s 0 (n - 2); op = `Insert (digit s.[n - 1]) }
    else invalid_arg __FUNCTION__
end

module Lens_System = struct
  type t = (string * int) list
end

module Boxes = struct
  type t = Lens_System.t array
end
