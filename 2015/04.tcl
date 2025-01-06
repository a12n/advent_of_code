#!/usr/bin/env tclsh

package require md5

set n [switch $puzzle(part) {
    1 { expr 5 }
    2 { expr 6 }
}]
set prefix [string repeat 0 $n]

set key [gets stdin]

# XXX
for {set i 0} {1} {incr i} {
    if {[string equal -length $n [md5::md5 -hex "$key$i"] $prefix]} {
        puts $i
        return
    }
}
