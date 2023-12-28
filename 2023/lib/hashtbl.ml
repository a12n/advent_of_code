include Stdlib.Hashtbl

module Ops = struct
  let ( .%{} ) = find
  let ( .%%{} ) = find_opt
  let ( .%{}<- ) = replace
  let ( .%%{}<- ) = add
end
