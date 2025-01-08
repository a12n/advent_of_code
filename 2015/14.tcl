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

set n [llength $reindeers]
set points [lrepeat $n 0]
for {set t 1} {$t <= $tEnd} {incr t} {
    set distances [lmap reindeer $reindeers { distance $reindeer $t }]
    set lead 0
    for {set i 1} {$i < $n} {incr i} {
        if {[lindex $distances $i] > [lindex $distances [lindex $lead 0]]} {
            set lead $i
        } elseif {[lindex $distances $i] == [lindex $distances [lindex $lead 0]]} {
            lappend lead $i
        }
    }
    foreach i $lead {
        lset points $i [expr {[lindex $points $i] + 1}]
    }
    puts stderr "$t: points $points"
}

puts [tcl::mathfunc::max {*}$points]
