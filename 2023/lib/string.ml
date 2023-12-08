include Stdlib.String

let to_list s = List.init (length s) (unsafe_get s)
