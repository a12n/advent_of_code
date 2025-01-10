#!/usr/bin/env tclsh

array set replacements {}
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    lappend replacements($from) $to
}

set molecule [gets stdin]

puts stderr "replacements [array get replacements], molecule $molecule"
