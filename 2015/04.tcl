#!/usr/bin/env tclsh

package require md5

proc prefixZeroes {key n {start 0} {step 1}} {
    set zeroes [string repeat 0 $n]

    for {set i $start} {1} {incr i $step} {
        if {[string equal -length $n [md5::md5 -hex "$key$i"] $zeroes]} {
            return $i
        }
    }
}

set key [gets stdin]
set n [switch $puzzle(part) {
    1 { expr 5 }
    2 { expr 6 }
}]

puts [prefixZeroes $key $n]
