open Advent

module Spring = struct
  let of_char = function '.' -> true | '#' -> false | _ -> invalid_arg __FUNCTION__
  let to_char = function true -> '.' | false -> '#'
  let opt_of_char = function '?' -> None | c -> Some (of_char c)
  let opt_to_char = function None -> '?' | Some s -> to_char s
end

module Pattern : sig
  type t = private bool option list * int list

  val arrangements : t -> int
  val of_string : string -> t
  val unfold : t -> t
end = struct
  type t = bool option list * int list

  let arrangements (springs, pattern) =
    let rec loop = function
      (* TODO *)
      |_,_,_ -> 0 in
    loop (springs, None, pattern)

  let of_string s =
    match String.split_on_char ' ' s |> List.map String.trim |> List.filter (( <> ) "") with
    | [ springs; numbers ] ->
        let springs = List.init (String.length springs) (fun i -> Spring.opt_of_char springs.[i]) in
        let pattern = String.split_on_char ',' numbers |> List.map int_of_string in
        (springs, pattern)
    | _ -> invalid_arg __FUNCTION__

  let unfold (springs, pattern) =
    let num_copies = 5 in
    let springs' = List.(concat (intersperse [ None ] (make num_copies springs))) in
    let pattern' = List.(concat (make num_copies pattern)) in
    (springs', pattern')
end
