include Stdlib.Int

let is_even n = n mod 2 = 0
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = abs a * (abs b / gcd a b)
