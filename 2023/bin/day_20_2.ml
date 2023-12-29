open Advent
open Day_20

let () =
  let cfg = Config.of_lines (input_lines stdin) in
  let (Stats.{ low; _ } as stats) = Stats.make () in
  Config.pp Format.err_formatter cfg;
  Seq.(ints 1)
  |> Seq.find (fun i ->
         Stats.clear stats;

         Config.push_button cfg stats;

         (* "rx" is low when all "&zh" inputs are high. "&zh" inputs
            are wired to "&jt", "&kb", "&sx" and "&ks" outputs. All these
            conjunctions (inverters) must see low pulse to set "&zh" to
            high, and "rx" to low. *)
         let check_low name =
           match Hashtbl.find_opt low name with
           | Some n -> Printf.eprintf "%d: %s %d\n%!" i name n
           | None -> ()
         in
         check_low "jt";
         check_low "kb";
         check_low "sx";
         check_low "ks";

         Hashtbl.mem low "rx")
  |> Option.get |> string_of_int |> print_endline
