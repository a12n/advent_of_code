let () =
  let is_digit c = c >= '0' && c <= '9' in
  let int_of_digit d = int_of_char d - int_of_char '0' in
  let sum, _ =
    Seq.of_dispenser (fun () ->
        match input_char stdin with
        | c -> Some c
        | exception End_of_file -> None)
    |> Seq.filter_map (function
         | '\n' -> Some (-1)
         | c when is_digit c -> Some (int_of_digit c)
         | _other -> None)
    |> Seq.fold_left
         (fun (sum, acc) k ->
           match (acc, k) with
           | None, -1 -> (sum, None)
           | None, k -> (sum, Some (k, k))
           | Some (n, m), -1 -> (sum + (n * 10) + m, None)
           | Some (n, _), k -> (sum, Some (n, k)))
         (0, None)
  in
  print_endline (string_of_int sum)
