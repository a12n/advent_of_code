open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  let (Stats.{ low; _ } as stats) = Stats.make () in
  (* "rx" is low when all "&zh" inputs are high. "&zh" inputs are
     wired to "&jt", "&kb", "&sx" and "&ks" outputs. All these
     conjunctions (inverters) must see low pulse to set "&zh" to high,
     and "rx" to low. It happens with some different period for each
     inverter. LCM of these periods is the answer. *)
  let multiples =
    List.map
      (fun name ->
        Config.reset cfg;
        Stats.clear stats;
        Seq.find
          (fun i ->
            Config.push_button cfg stats;
            if Hashtbl.mem low name then (
              Printf.eprintf "%d: low on %s\n%!" i name;
              true)
            else false)
          (Seq.ints 1)
        |> Option.get)
      [ "jt"; "kb"; "sx"; "ks" ]
  in
  print_endline (string_of_int (List.reduce Int.lcm multiples))
