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

puts stderr "reindeers $reindeers"

puts [tcl::mathfunc::max {*}[lmap reindeer $reindeers { distance $reindeer $tEnd }]]

set stopFlying {}
foreach reindeer $reindeers {
    lassign $reindeer name speed fly rest

    lappend stopFlying $fly
    for {set k 1} {1} {incr k} {
        set t [expr {$k * ($rest + $fly)}]
        if {($t - $fly) >= $tEnd} {
            break
        }
        lappend stopFlying $t
    }
}

puts stderr "stopFlying [lsort -integer -unique $stopFlying]"
