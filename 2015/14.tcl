#!/usr/bin/env tclsh

set duration 2503
catch { set duration $env(DURATION) }

while {[gets stdin line] >= 0} {
    lassign [string map {
        " can fly" ""
        " km/s for" ""
        " seconds" ""
        " but then must rest for" ""
        "," ""
        "." ""
    } $line] name speed fly rest

    lappend reindeers [list $speed $fly $rest]
}

set best 0
foreach reindeer $reindeers {
    lassign $reindeer speed fly rest
    set cycle [expr {$fly + $rest}]
    set n [expr {$duration / $cycle}]
    set rem [expr {$duration % $cycle}]
    set dist [expr {$speed * ($fly * $n + min($rem, $fly))}]
    set best [expr {max($best, $dist)}]
}
puts $best
