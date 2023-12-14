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

  let disjoint s t = t.min > s.max || s.min > t.max

  let inter s t =
    if disjoint s t then None
    else Some { min = Int.max s.min t.min; max = Int.min s.max t.max }

  let union s t =
    if disjoint s t then None
    else Some { min = Int.min s.min t.min; max = Int.max s.max t.max }

  let diff s t =
    match inter s t with
    | Some { min; max } ->
        let left_of_t =
          if min > s.min then [ { min = s.min; max = min - 1 } ] else []
        in
        let right_of_t =
          if max < s.max then [ { min = max + 1; max = s.max } ] else []
        in
        left_of_t @ right_of_t
    | None -> [ s ]

  let length { min; max } = max - min + 1
end

module Mapping : sig
  type t = private (Segment.t * int) list

  val empty : t
  val find : t -> Segment.t -> Segment.t list
  val of_lines : string Seq.t -> t
end = struct
  type t = (Segment.t * int) list

  let empty = []

  let of_lines lines =
    let parse_line assoc line =
      match
        String.split_on_char ' ' line
        |> List.filter (( <> ) "")
        |> List.map int_of_string
      with
      | [ dest; src; len ] -> (Segment.make src (`Length len), dest) :: assoc
      | _ -> invalid_arg __FUNCTION__
    in
    Seq.take_while (( <> ) "") lines
    |> Seq.fold_left parse_line empty
    |> List.stable_sort Stdlib.compare

  let rec find assoc q =
    match assoc with
    | [] ->
        (* No segments in the mapping intersects the queried
           segment. Just return the query. *)
        [ q ]
    | (src, _) :: _ when Segment.(src.min > q.max) ->
        (* The queried segment is entirely left to the first segment
           in the mapping, no intersection possible. Just return the
           query itself. *)
        [ q ]
    | (src, dest) :: assoc' -> (
        match Segment.inter q src with
        | Some overlap ->
            (* Remap intersection as the destination segment. *)
            let r0 =
              Segment.make
                (dest + overlap.min - src.min)
                (`Length (Segment.length overlap))
            in
            (* Try to intersect non-overlaped parts of the query
               segment to the other segments in the mapping. *)
            let rs =
              match Segment.diff q overlap with
              | [] -> []
              | [ q' ] | [ _; q' ] -> find assoc' q'
              | _ -> failwith "unreachable"
            in
            r0 :: rs
        | None ->
            (* The queried segment doesn't intersect with the current
               segment, but may intersect other segments in the list. *)
            find assoc' q)
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
  val seed_to_location : t -> Segment.t list -> Segment.t list
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

  let seed_to_location almanac seeds =
    let find_segments map segs =
      List.flatten (List.map (Mapping.find map) segs)
    in
    seeds
    |> find_segments almanac.seed_to_soil
    |> find_segments almanac.soil_to_fertilizer
    |> find_segments almanac.fertilizer_to_water
    |> find_segments almanac.water_to_light
    |> find_segments almanac.light_to_temperature
    |> find_segments almanac.temperature_to_humidity
    |> find_segments almanac.humidity_to_location
end
