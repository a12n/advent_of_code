let update_hash h c = 17 * (h + int_of_char c) mod 256
let hash = String.fold_left update_hash 0
