open Day_22

let () =
  let bricks = input stdin in
  Format.(pp_print_list ~pp_sep:pp_print_space Brick.pp err_formatter bricks)
