open Advent

module Numbers : sig
  include module type of Set.Make (Int)

  val of_string : string -> t
end = struct
  include Set.Make (Int)

  let of_string s =
    String.split_on_char ' ' s
    |> List.filter (( <> ) "")
    |> List.map int_of_string |> of_list
end

module Card : sig
  type t = int * Numbers.t

  val of_string : string -> t
end = struct
  type t = int * Numbers.t

  let of_string s =
    match String.split_on_char ':' s with
    | [ header; numbers ] -> (
        match String.split_on_char ' ' header |> List.filter (( <> ) "") with
        | [ "Card"; id ] -> (
            match String.split_on_char '|' numbers with
            | [ winning; have ] ->
                ( int_of_string id,
                  Numbers.(inter (of_string winning) (of_string have)) )
            | _ -> invalid_arg __FUNCTION__)
        | _ -> invalid_arg __FUNCTION__)
    | _ -> invalid_arg __FUNCTION__
end

let part1 () =
  let sum =
    input_lines stdin |> Seq.map Card.of_string
    |> Seq.map (fun (_, matching) ->
           match Numbers.cardinal matching with 0 -> 0 | n -> 1 lsl (n - 1))
    |> Seq.reduce ( + )
  in
  print_endline (string_of_int sum)

let part2 () =
  let matching =
    input_lines stdin |> Seq.map Card.of_string
    |> Seq.mapi (fun i (id, matching) ->
           assert (i = id - 1);
           Numbers.cardinal matching)
    |> Array.of_seq
  in
  let n = Array.length matching in
  let copies = Array.make n 1 in
  for i = 0 to n - 1 do
    for j = 1 to matching.(i) do
      copies.(i + j) <- copies.(i + j) + copies.(i)
    done
  done;
  print_endline (string_of_int (Array.reduce ( + ) copies))

let () = (parse_args Sys.argv [| part1; part2 |]) ()