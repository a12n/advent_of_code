#!/usr/bin/env tclsh

set liters 150
catch { set liters $env(LITERS) }
puts stderr "liters $liters"

set capacities [lsort -decreasing -integer [read stdin]]
puts stderr "capacities $capacities"

proc combinations {capacities liters} {
    puts stderr "combinations: capacities $capacities liters $liters"

    if {$liters == 0} {
        return 1
    } elseif {$capacities eq {}} {
        return 0
    }

    set capacities2 [lassign $capacities cap]
    set sum 0

    if {$liters >= $cap} {
        incr sum [combinations $capacities2 [expr {$liters - $cap}]]
    }
    incr sum [combinations $capacities2 $liters]

    return $sum
}

puts [combinations $capacities $liters]
