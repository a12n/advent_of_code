#!/usr/bin/env tclsh

set replacements [dict create]
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    dict update replacements $from toList {
        lappend toList $to
    }
}

set molecule [gets stdin]

puts stderr "replacements $replacements [dict size $replacements], molecule $molecule [string length $molecule]"

set replaced [dict create]
dict for {from toList} $replacements {
    set n [string length $from]
    set first 0
    while {[set first [string first $from $molecule $first]] != -1} {
        set last [expr {$first + $n - 1}]
        foreach to $toList {
            dict set replaced [string replace $molecule $first $last $to] yes
        }
        incr first
    }
}

puts stderr "replaced [lsort -index 0 -stride 2 $replaced] [dict size $replaced]"

puts [dict size $replaced]
