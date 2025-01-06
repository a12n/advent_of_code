#!/usr/bin/env tclsh

proc transform digits {
    set length 0
    set prev {}
    set result {}

    foreach digit $digits {
        if {$prev eq $digit || $prev eq ""} {
            incr length
        } else {
            if {$length > 0} {
                lappend result $length $prev
            }
            set length 1
        }
        set prev $digit
    }

    if {$length > 0} {
        lappend result $length $prev
    }

    return $result
}

switch $puzzle(part) {
    1 { set n 40 }
    2 { set n 50 }
}

set sequence [split [gets stdin] ""]
for {set i 0} {$i < $n} {incr i} {
    set sequence [transform $sequence]
}
puts [llength $sequence]
