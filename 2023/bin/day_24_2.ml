(* Find position [p] and velocty [v], such that
   for sample input:

   [19 13 30] + t1 [-2  1 -2] = p + t1 v
   [18 19 22] + t2 [-1 -1 -2] = p + t2 v
   [20 25 34] + t3 [-2 -2 -4] = p + t3 v
   [12 31 28] + t4 [-1 -2 -1] = p + t4 v
   [20 19 15] + t5 [ 1 -5 -3] = p + t5 v

   t = [t1 t2 t3 t4 t5] = [5 3 4 6 1]

   p = [19 13 30] + t1 ([-2  1 -2] - v)
   p = [18 19 22] + t2 ([-1 -1 -2] - v)
   p = [20 25 34] + t3 ([-2 -2 -4] - v)
   p = [12 31 28] + t4 ([-1 -2 -1] - v)
   p = [20 19 15] + t5 ([ 1 -5 -3] - v)


   19 - 2 t1 = x + u t1
   13 + 1 t1 = y + v t1
   30 - 2 t1 = z + w t1

   t1 = (x - 19) / (-2 - u)
   t1 = (y - 13) / (1 - v)
   t1 = (z - 30) / (-2 - w)

   (x - 19) / (-2 - u) = (y - 13) / (1 - v) = (z - 30) / (-2 - w)


   18 - 1 t2 = x + u t2
   19 - 1 t2 = y + v t2
   22 - 2 t2 = z + w t2

   t2 = (x - 18) / (-1 - u)
   t2 = (y - 19) / (-1 - v)
   t2 = (z - 22) / (-2 - w)

   (x - 18) / (-1 - u) = (y - 19) / (-1 - v) = (z - 22) / (-2 - w)


   20 - 2 t3 = x + u t3
   25 - 2 t3 = y + v t3
   34 - 4 t3 = z + w t3

   t3 = (x - 20) / (-2 - u)
   t3 = (y - 25) / (-2 - v)
   t3 = (z - 34) / (-4 - w)

   (x - 20) / (-2 - u) = (y - 25) / (-2 - v) = (z - 34) / (-4 - w)


   (x - 19) / (-2 - u) = (y - 13) / (1 - v) = (z - 30) / (-2 - w)
   (x - 18) / (-1 - u) = (y - 19) / (-1 - v) = (z - 22) / (-2 - w)
   (x - 20) / (-2 - u) = (y - 25) / (-2 - v) = (z - 34) / (-4 - w)


   (x - 19) / (-2 - u) = (y - 13) / ( 1 - v)
   (x - 19) / (-2 - u) = (z - 30) / (-2 - w)
   (y - 13) / ( 1 - v) = (z - 30) / (-2 - w)

   (x - 18) / (-1 - u) = (y - 19) / (-1 - v)
   (x - 18) / (-1 - u) = (z - 22) / (-2 - w)
   (y - 19) / (-1 - v) = (z - 22) / (-2 - w)

   (x - 20) / (-2 - u) = (y - 25) / (-2 - v)
   (x - 20) / (-2 - u) = (z - 34) / (-4 - w)
   (y - 25) / (-2 - v) = (z - 34) / (-4 - w)


   ( 1 - v) (x - 19) = (-2 - u) (y - 13)
   (-2 - w) (x - 19) = (-2 - u) (z - 30)
   (-2 - w) (y - 13) = ( 1 - v) (z - 30)

   (-1 - v) (x - 18) = (-1 - u) (y - 19)
   (-2 - w) (x - 18) = (-1 - u) (z - 22)
   (-2 - w) (y - 19) = (-1 - v) (z - 22)

   (-2 - v) (x - 20) = (-2 - u) (y - 25)
   (-4 - w) (x - 20) = (-2 - u) (z - 34)
   (-4 - w) (y - 25) = (-2 - v) (z - 34)


      x -  19 - v x + 19 v = -2 y + 26 - u y + 13 u
   -2 x +  38 - w x + 19 w = -2 z + 60 - u z + 30 u
   -2 y +  26 - w y + 13 w =    z - 30 - v z + 30 v

     -x +  18 - v x + 18 v =   -y + 19 - u y + 19 u
   -2 x +  36 - w x + 18 w =   -z + 22 - u z + 22 u
   -2 y +  38 - w y + 19 w =   -z + 22 - v z + 22 v

   -2 x +  40 - v x + 20 v = -2 y + 50 - u y + 25 u
   -4 x +  80 - w x + 20 w = -2 z + 68 - u z + 34 u
   -4 y + 100 - w y + 25 w = -2 z + 68 - v z + 34 v
*)
open Advent
open Day_24

let with_debug =
  if Option.is_some (Sys.getenv_opt "DEBUG") then fun f -> f Format.err_formatter else fun _ -> ()

let rock_position hailstones v' =
  try
    List.combine_tl hailstones
    |> List.fold_lefti
         (fun ans i ((pi, vi), hs) ->
           let ((pi, vi) as hi) = (pi, Vector.sub vi v') in
           with_debug
             Format.(
               fun fmt ->
                 fprintf fmt "Intersect %d " i;
                 Hailstone.pp fmt hi;
                 pp_print_string fmt " with:";
                 pp_print_newline fmt ());
           List.fold_lefti
             (fun ans j (pj, vj) ->
               let hj = (pj, Vector.sub vj v') in
               let ixn = Point.intersect hi hj in
               with_debug
                 Format.(
                   fun fmt ->
                     fprintf fmt "\t%d " (j + i + 1);
                     Hailstone.pp fmt hj;
                     pp_print_string fmt " -> ");
               match ixn with
               | None ->
                   with_debug
                     Format.(
                       fun fmt ->
                         pp_print_string fmt "None, Exit";
                         pp_print_newline fmt ());
                   raise Exit
               | Some (t1, _) -> (
                   let p' = Point.add pi (Vector.mul_elt vi t1) in
                   with_debug (fun fmt -> Point.pp fmt p');
                   match ans with
                   | None ->
                       with_debug
                         Format.(
                           fun fmt ->
                             pp_print_string fmt ", OK";
                             pp_print_newline fmt ());
                       Some p'
                   | Some p'' when Point.equal p' p'' ->
                       with_debug
                         Format.(
                           fun fmt ->
                             pp_print_string fmt ", OK";
                             pp_print_newline fmt ());
                       ans
                   | Some _ ->
                       with_debug
                         Format.(
                           fun fmt ->
                             pp_print_string fmt ", Exit";
                             pp_print_newline fmt ());
                       raise Exit))
             ans hs)
         None
  with Exit -> None

let () =
  let hailstones = of_lines (input_lines stdin) in
  let v = Vector.{ x = Q.of_int (-3); y = Q.of_int 1; z = Q.of_int 2 } in
  let p = rock_position hailstones v in
  Point.reduce Q.add (Option.get p) |> Q.to_string |> print_endline
