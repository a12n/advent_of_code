open Day_22

let () =
  let bricks = input stdin in
  Brick.pp_list Format.err_formatter bricks
