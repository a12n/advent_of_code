#!/usr/bin/env tclsh

set ingredients {}
while {[gets stdin line] >= 0} {
    set ingredient [lrange [string map {, {} : {}} $line] 1 end]
    lappend ingredients [lsort -index 0 -stride 2 $ingredient]
}

proc multipliy {nums ingredients} {
    set n [llength $ingredients]
    set prod {capacity 0 durability 0 flavor 0 texture 0}

    for {set i 0} {$i < 4} {incr i} {
        # Index of property with stride 2.
        set i2 [expr {2 * $i + 1}]

        for {set j 0} {$j < $n} {incr j} {
            set x [lindex $nums $j]
            set a [lindex [lindex $ingredients $j] $i2]
            lset prod $i2 [expr {[lindex $prod $i2] + $x * $a}]
        }
    }

    return $prod
}

switch $puzzle(part) {
    1 {
        # Filter out calories for part 1.
        set ingredients [lmap ingredient $ingredients { dict remove $ingredient calories }]
        puts stderr "ingredients $ingredients"
        puts stderr "multipliy [multipliy {44 56} $ingredients]"
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
