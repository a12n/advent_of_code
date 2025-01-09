#!/usr/bin/env tclsh

set ingredients {}
while {[gets stdin line] >= 0} {
    set ingredient [lrange [string map {, {} : {}} $line] 1 end]
    lappend ingredients [lsort -index 1 -stride 2 $ingredient]
}

switch $puzzle(part) {
    1 {
        # Filter out calories for part 1.
        set ingredients [lmap ingredient $ingredients { dict remove $ingredient calories }]
        puts stderr "ingredients $ingredients"
    }
}

# N = number of ingredients
# A = [
#   capacity1   capacity2 …   capacityN;
# durability1 durability2 … durabilityN;
#     flavor1     flavor2 …     flavorN;
#    texture1    texture2 …    textureN
# ]
# x = [x1; x2; …; xN]
# A x = B = [capacity; durability; flavor; texture]
# max Π B
