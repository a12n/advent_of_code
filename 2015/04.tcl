#!/usr/bin/env tclsh

package require Thread

set key [gets stdin]
set n [switch $puzzle(part) {
    1 { expr 5 }
    2 { expr 6 }
}]

for {set i 0} {$i < 8} {incr i} {
    set id [thread::create {
        package require md5

        proc prefixZeroes {key n {start 0} {step 1}} {
            set zeroes [string repeat 0 $n]
            for {set i $start} {1} {incr i $step} {
                if {[string equal -length $n [md5::md5 -hex "$key$i"] $zeroes]} {
                    return $i
                }
            }
        }

        thread::wait
    }]

    thread::send -async $id [subst {prefixZeroes $key $n $i 8}] result
}

vwait result

puts $result
