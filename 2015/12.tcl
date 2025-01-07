#!/usr/bin/env tclsh

set sum 0

set accum 0
set accumSign 1

while {1} {
    switch -glob -- [set char [read stdin 1]] {
        {} { break }
        {-} { set accumSign -1 }
        {[0-9]} { set accum [expr {$accum * 10 + $char}] }
        default {
            if {$accum > 0} {
                incr sum [expr {$accumSign * $accum}]
            }
            set accum 0
            set accumSign 1
        }
    }
}

puts $sum
