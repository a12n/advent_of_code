module Ops = struct
  let ( .%{} ) = Hashtbl.find
  let ( .%%{} ) = Hashtbl.find_opt
  let ( .%{}<- ) = Hashtbl.replace
end
