#!/usr/bin/env tclsh

array set replacements {}
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    lappend replacements($from) $to
}

set molecule [gets stdin]

puts stderr "replacements [array get replacements] [array size replacements], molecule $molecule [string length $molecule]"

array set replaced {}
foreach {from toList} [array get replacements] {
    set n [string length $from]
    set first 0
    while {[set first [string first $from $molecule $first]] != -1} {
        set last [expr {$first + $n - 1}]
        foreach to $toList {
            set replaced([string replace $molecule $first $last $to]) yes
        }
        incr first
    }
}

puts stderr "replaced [lsort -index 0 -stride 2 [array get replaced]] [array size replaced]"

puts [array size replaced]
