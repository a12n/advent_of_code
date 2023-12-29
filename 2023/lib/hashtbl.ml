include Stdlib.Hashtbl

let modify ?default tbl f key =
  match (find_opt tbl key, default) with
  | Some value, _ -> replace tbl key (f value)
  | None, Some default -> replace tbl key default
  | None, None -> raise Not_found

module Ops = struct
  let ( .%{} ) = find
  let ( .%%{} ) = find_opt
  let ( .%{}<- ) = replace
  let ( .%%{}<- ) = add
end
