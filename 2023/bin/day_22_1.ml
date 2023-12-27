open Day_22

let () =
  let bricks = Snapshot.of_lines (input_lines stdin) in
  Format.(
    pp_print_string err_formatter "Input:\n";
    Snapshot.pp err_formatter bricks)
