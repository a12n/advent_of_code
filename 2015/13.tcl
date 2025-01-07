#!/usr/bin/env tclsh

set guests [dict create]

while {[gets stdin line] >= 0} {
    lassign [string map {
        "would gain " "+"
        "would lose " "-"
        "happiness units by sitting next to " ""
        "." ""
    } $line] guest happiness neighbor

    puts stderr "line \"$line\""
    puts stderr "guest \"$guest\", happiness $happiness, neighbor \"$neighbor\""

    dict set guests $guest $neighbor $happiness
}

puts stderr "guests $guests"

set pairs {}
foreach guest [dict keys $guests] {
    foreach neighbor [dict keys $guests] {
        if {$guest >= $neighbor} {
            continue
        }

        set happiness [expr {[dict get $guests $guest $neighbor] + [dict get $guests $neighbor $guest]}]
        puts stderr "$guest $neighbor = $happiness"

        lappend pairs [list $happiness $guest $neighbor]
    }
}
puts stderr "pairs [lsort -decreasing -index 0 -integer $pairs]"
