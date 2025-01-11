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

switch $puzzle(part) {
    1 {
        puts [llength [combinations $capacities $liters]]
    }
    2 {
        set freqs [dict create]
        foreach comb [combinations $capacities $liters] {
            set n [llength $comb]
            dict incr freqs $n
            if {![info exists minN] || $n < $minN} {
                set minN $n
            }
        }
        puts stderr "freqs $freqs"
        puts [dict get $freqs $minN]
    }
}
