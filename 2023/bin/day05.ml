open Advent

module Mapping : sig
  type t = private (int * (int * int)) list

  val add : t -> ?len:int -> int -> int -> t
  val empty : t
  val find : t -> int -> int
end = struct
  type t = (int * (int * int)) list

  (* TODO: Replace. *)
  let add assoc ?(len = 1) key value = (key, (value, len)) :: assoc
  let empty = []

  let find_last_opt assoc pred =
    List.fold_left
      (fun acc (k, v) ->
        match (pred k, acc) with
        | true, None -> Some (k, v)
        | true, Some (k', _) when k > k' -> Some (k, v)
        | _, acc -> acc)
      None assoc

  let find assoc k =
    (* Find greatest key less than or equal to [k]. *)
    match find_last_opt assoc (( >= ) k) with
    | Some (k', (v, len)) when k < k' + len ->
        (* If the requested key is withing the range, map to destination
           value. *)
        v + (k - k')
    | Some _ | None ->
        (* Any source numbers that aren't mapped correspond to the same
           destination number. *)
        k
end

module Almanac : sig
  type t

  val of_lines : string Seq.t -> t
end = struct
  type t = {
    seeds : int list;
    seed_to_soil : Mapping.t;
    soil_to_fertilizer : Mapping.t;
    fertilizer_to_water : Mapping.t;
    water_to_light : Mapping.t;
    light_to_temperature : Mapping.t;
    temperature_to_humidity : Mapping.t;
    humidity_to_location : Mapping.t;
  }

  let empty =
    {
      seeds = [];
      seed_to_soil = Mapping.empty;
      soil_to_fertilizer = Mapping.empty;
      fertilizer_to_water = Mapping.empty;
      water_to_light = Mapping.empty;
      light_to_temperature = Mapping.empty;
      temperature_to_humidity = Mapping.empty;
      humidity_to_location = Mapping.empty;
    }

  let of_lines _lines = empty
end

let part1 () =
  let _almanac = Almanac.of_lines (input_lines stdin) in
  ()

let part2 () = ()
let () = (parse_args Sys.argv [| part1; part2 |]) ()
