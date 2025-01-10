#!/usr/bin/env tclsh

array set replacements {}
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    lappend replacements($from) $to
}

set molecule [gets stdin]

puts stderr "replacements [array get replacements], molecule $molecule"

set replaced {}
foreach {from toList} [array get replacements] {
    set pos 0
    while {[set pos [string first $from $molecule $pos]] != -1} {
        foreach to $toList {
            dict set replaced [list $pos $to] yes
        }
        incr pos
    }
}

puts stderr "replaced $replaced"
