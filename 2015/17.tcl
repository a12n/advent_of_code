#!/usr/bin/env tclsh

set liters 150
catch { set liters $env(LITERS) }
puts stderr "liters $liters"

set capacities [lsort -decreasing -integer [read stdin]]
puts stderr "capacities $capacities"

proc combinations {capacities liters {used {}}} {
    puts stderr "combinations: capacities $capacities liters $liters"

    if {$liters == 0} {
        puts stderr "combinations: used $used"
        return [list $used]
    } elseif {$liters < 0 || $capacities eq {}} {
        return {}
    }

    set capacitiesLeft [lassign $capacities cap]

    concat \
        [combinations $capacitiesLeft [expr {$liters - $cap}] [lreplace $used end+1 end+1 $cap]] \
        [combinations $capacitiesLeft $liters $used]
}

puts [llength [combinations $capacities $liters]]
