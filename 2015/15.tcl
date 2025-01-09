#!/usr/bin/env tclsh

set ingredients {}
while {[gets stdin line] >= 0} {
    set ingredient [lrange [string map {, {} : {}} $line] 1 end]
    lappend ingredients [lsort -index 0 -stride 2 $ingredient]
}

switch $puzzle(part) {
    1 {
        # Filter out calories for part 1.
        set ingredients [lmap ingredient $ingredients { dict remove $ingredient calories }]
        puts stderr "ingredients $ingredients"
    }
}

# N = number of ingredients
#
# x = [x1 x2 … xN]
# A = [
# capacity1 durability1 flavor1 texture1;
# capacity2 durability2 flavor2 texture2;
# …
# capacityN durabilityN flavorN textureN
# ]
# x A = B = [capacity durability flavor texture]
#
# Σ x = 100
# max Π B
