module String = Advent__String

let input_lines ch =
  Seq.of_dispenser (fun () ->
      match input_line ch with l -> Some l | exception End_of_file -> None)
