open Advent

module Segment : sig
  type t = private { min : int; max : int }

  val make : int -> [ `End of int | `Length of int ] -> t
  val length : t -> int
  val inter : t -> t -> t option
  val union : t -> t -> t option
  val diff : t -> t -> t list
end = struct
  type t = { min : int; max : int }

  let make min = function
    | `End max when max >= min -> { min; max }
    | `Length 0 -> { min; max = min }
    | `Length n when n > 0 -> { min; max = min + n - 1 }
    | _ -> invalid_arg __FUNCTION__

  let disjoint i1 i2 = i2.min > i1.max || i1.min > i2.max

  let inter i1 i2 =
    if disjoint i1 i2 then None
    else Some { min = Int.max i1.min i2.min; max = Int.min i1.max i2.max }

  let union i1 i2 =
    if disjoint i1 i2 then None
    else Some { min = Int.min i1.min i2.min; max = Int.max i1.max i2.max }

  let diff i1 i2 =
    match inter i1 i2 with
    | Some { min; max } ->
        (if min > i1.min then [ { min = i1.min; max = min - 1 } ] else [])
        @ if max < i1.max then [ { min = max + 1; max = i1.max } ] else []
    | None -> [ i1 ]

  let length { min; max } = max - min + 1
end

module Mapping : sig
  type t = private (int * int * int) list

  val empty : t
  val find : t -> int -> int
  val of_lines : string Seq.t -> t
end = struct
  type t = (int * int * int) list

  (* TODO: Replace. *)
  let add assoc ?(len = 1) src dest = (src, dest, len) :: assoc
  let empty = []

  let find_last_opt assoc pred =
    List.fold_left
      (fun acc (src, dest, len) ->
        match (pred src, acc) with
        | true, None -> Some (src, dest, len)
        | true, Some (src', _, _) when src > src' -> Some (src, dest, len)
        | _, acc -> acc)
      None assoc

  let find assoc src =
    (* Find greatest src less than or equal to [src]. *)
    match find_last_opt assoc (( >= ) src) with
    | Some (src', dest, len) when src < src' + len ->
        (* If the requested source is withing the range, map to destination. *)
        dest + (src - src')
    | Some _ | None ->
        (* Any source numbers that aren't mapped correspond to the same
           destination number. *)
        src

  let of_lines lines =
    let parse_line assoc line =
      match
        String.split_on_char ' ' line
        |> List.filter (( <> ) "")
        |> List.map int_of_string
      with
      | [ dest; src; len ] -> add assoc ~len src dest
      | _ -> invalid_arg __FUNCTION__
    in
    Seq.take_while (( <> ) "") lines
    |> Seq.fold_left parse_line empty
    |> List.stable_sort Stdlib.compare
end

module Almanac : sig
  type t = private {
    seeds : (int * int) list;
    seed_to_soil : Mapping.t;
    soil_to_fertilizer : Mapping.t;
    fertilizer_to_water : Mapping.t;
    water_to_light : Mapping.t;
    light_to_temperature : Mapping.t;
    temperature_to_humidity : Mapping.t;
    humidity_to_location : Mapping.t;
  }

  val of_lines : string Seq.t -> t
  val seed_to_location : t -> int -> int
end = struct
  type t = {
    seeds : (int * int) list;
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

  let of_lines =
    let rec parse almanac lines =
      let parse_seeds s =
        match
          String.split_on_char ' ' s
          |> List.filter (( <> ) "")
          |> List.map int_of_string |> List.pairs
        with
        | [] -> invalid_arg (__FUNCTION__ ^ ": empty seeds")
        | l -> l
      in
      match Seq.uncons lines with
      | Some (line, lines') -> (
          Printf.eprintf "line \"%s\"\n%!" line;
          match String.split_on_char ':' line with
          | [ "seeds"; seeds ] ->
              parse { almanac with seeds = parse_seeds seeds } lines'
          | [ "seed-to-soil map"; "" ] ->
              parse
                { almanac with seed_to_soil = Mapping.of_lines lines' }
                lines'
          | [ "soil-to-fertilizer map"; "" ] ->
              parse
                { almanac with soil_to_fertilizer = Mapping.of_lines lines' }
                lines'
          | [ "fertilizer-to-water map"; "" ] ->
              parse
                { almanac with fertilizer_to_water = Mapping.of_lines lines' }
                lines'
          | [ "water-to-light map"; "" ] ->
              parse
                { almanac with water_to_light = Mapping.of_lines lines' }
                lines'
          | [ "light-to-temperature map"; "" ] ->
              parse
                { almanac with light_to_temperature = Mapping.of_lines lines' }
                lines'
          | [ "temperature-to-humidity map"; "" ] ->
              parse
                {
                  almanac with
                  temperature_to_humidity = Mapping.of_lines lines';
                }
                lines'
          | [ "humidity-to-location map"; "" ] ->
              parse
                { almanac with humidity_to_location = Mapping.of_lines lines' }
                lines'
          | [ "" ] -> parse almanac lines'
          | _ -> invalid_arg (__FUNCTION__ ^ ": invalid line \"" ^ line ^ "\""))
      | None -> almanac
    in
    parse empty

  let seed_to_location almanac seed =
    Mapping.find almanac.seed_to_soil seed
    |> Mapping.find almanac.soil_to_fertilizer
    |> Mapping.find almanac.fertilizer_to_water
    |> Mapping.find almanac.water_to_light
    |> Mapping.find almanac.light_to_temperature
    |> Mapping.find almanac.temperature_to_humidity
    |> Mapping.find almanac.humidity_to_location
end
