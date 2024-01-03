open Advent
open Day_25

let () =
  let conns = Config.of_lines (input_lines stdin) in
  Format.(
    fprintf err_formatter "// %d conns\n" (Hashtbl.length conns);
    Config.pp err_formatter conns
  )
