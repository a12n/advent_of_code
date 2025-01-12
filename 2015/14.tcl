#!/usr/bin/env tclsh

# Absolute position offset of the reindeer after t seconds.
proc distance {reindeer t} {
    lassign $reindeer name speed fly rest
    set cycle [expr {$fly + $rest}]
    set n [expr {$t / $cycle}]
    set rem [expr {$t % $cycle}]
    expr {$speed * ($fly * $n + min($rem, $fly))}
}

# Time instants (under the specified total time limit) at which the
# reindeer will stop for rest.
proc stops {reindeer duration} {
    lassign $reindeer name speed fly rest
    set times $fly
    for {set i 1} {1} {incr i} {
        set t [expr {$i * ($rest + $fly)}]
        if {($t - $fly) >= $duration)} {
            break
        }
        lappend times $t
    }
    return $times
}

proc 1 {reindeers tEnd} {
    puts [tcl::mathfunc::max {*}[lmap reindeer $reindeers { distance $reindeer $tEnd }]]
}

proc 2 {reindeers tEnd} {
    set n [llength $reindeers]
    set scores [lrepeat $n 0]

    # TODO: Evaluate only at stop time instants?
    for {set t 1} {$t <= $tEnd} {incr t} {
        set distances [lmap reindeer $reindeers { distance $reindeer $t }]
        set leaders 0

        for {set i 1} {$i < $n} {incr i} {
            set reindeer [lindex $reindeers $i]
            set best [lindex $distances [lindex $leaders 0]]
            set this [lindex $distances $i]

            if {$this > $best} {
                set leaders $i
            } elseif {$this == $best} {
                lappend leaders $i
            }
        }

        foreach i $leaders {
            lset scores $i [expr {[lindex $scores $i] + 1}]
        }
    }

    puts [tcl::mathfunc::max {*}$scores]
}

# Total time limit.
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

$puzzle(part) $reindeers $tEnd
