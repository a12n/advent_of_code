#!/usr/bin/env tclsh

set tEnd 2503
catch { set tEnd $env(DURATION) }

proc distance {reindeer t} {
    lassign $reindeer name speed fly rest
    set cycle [expr {$fly + $rest}]
    set n [expr {$t / $cycle}]
    set rem [expr {$t % $cycle}]
    expr {$speed * ($fly * $n + min($rem, $fly))}
}

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

puts [tcl::mathfunc::max {*}[lmap reindeer $reindeers { distance $reindeer $tEnd }]]
