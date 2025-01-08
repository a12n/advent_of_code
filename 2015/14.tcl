#!/usr/bin/env tclsh

set tEnd 2503
catch { set tEnd $env(DURATION) }

while {[gets stdin line] >= 0} {
    lassign [string map {
        " can fly" ""
        " km/s for" ""
        " seconds" ""
        " but then must rest for" ""
        "," ""
        "." ""
    } $line] name speed fly rest

    lappend reindeers [list $name $speed $fly $rest]
}

set best 0
foreach reindeer $reindeers {
    lassign $reindeer name speed fly rest
    set cycle [expr {$fly + $rest}]
    set n [expr {$tEnd / $cycle}]
    set rem [expr {$tEnd % $cycle}]
    set dist [expr {$speed * ($fly * $n + min($rem, $fly))}]
    set best [expr {max($best, $dist)}]
}
puts $best
