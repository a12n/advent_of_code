open Advent

module Mapping : sig
  type t = private (int * (int * int)) list

  val empty : t
  val find : t -> int -> int
  val of_lines : string Seq.t -> t
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

  let of_lines lines =
    let parse_line assoc line =
      match
        String.split_on_char ' ' line
        |> List.filter (( <> ) "")
        |> List.map int_of_string
      with
      | [ dest; source; len ] -> add assoc ~len source dest
      | _ -> invalid_arg __FUNCTION__
    in
    Seq.take_while (( <> ) "") lines |> Seq.fold_left parse_line empty
end

module Almanac : sig
  type t = private {
    seeds : int list;
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

  let of_lines =
    let rec parse almanac lines =
      let parse_seeds s =
        match
          String.split_on_char ' ' s
          |> List.filter (( <> ) "")
          |> List.map int_of_string
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

let part1 () =
  let _almanac = Almanac.of_lines (input_lines stdin) in
  ()

let part2 () = ()
let () = (parse_args Sys.argv [| part1; part2 |]) ()
