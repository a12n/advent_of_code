include Stdlib.Int

let is_even n = n mod 2 = 0
let is_odd n = not (is_even n)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = abs a * (abs b / gcd a b)
let sqr n = n * n

(** Raise [n] to non-negative power [m]. *)
let rec pow n = function
  | 0 -> 1
  | 1 -> n
  | 2 -> sqr n
  | 3 -> n * n * n
  | m when is_even m -> sqr (pow n (m / 2))
  | m -> n * pow n (m - 1)
