open Day_22

let () =
  let bricks = input stdin in
  Format.(
    pp_print_list ~pp_sep:pp_print_newline Brick.pp err_formatter bricks;
    pp_print_newline err_formatter ())
