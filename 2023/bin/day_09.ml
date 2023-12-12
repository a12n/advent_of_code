let input chan =
  Advent.input_lines chan
  |> Seq.map (String.split_on_char ' ')
  |> Seq.map (List.filter (( <> ) ""))
  |> Seq.map (List.map int_of_string)
