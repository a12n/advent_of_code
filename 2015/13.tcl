#!/usr/bin/env tclsh

set happiness [dict create]

while {[gets stdin line] >= 0} {
    lassign [string map {
        "would gain " "+"
        "would lose " "-"
        "happiness units by sitting next to " ""
        "." ""
    } $line] guest gain neighbor

    dict set happiness $guest $neighbor $gain
}

proc permutations elts {
    set n [llength $elts]

    if {$n < 2} {
        return $elts
    }

    set ans {}

    for {set i 0} {$i < $n} {incr i} {
        set elt [lindex $elts $i]
        foreach sub [permutations [lreplace $elts $i $i]] {
            lappend ans [concat $elt $sub]
        }
    }

    return $ans
}

proc pairwise {happiness first second} {
    expr {[dict get $happiness $first $second] + [dict get $happiness $second $first]}
}

proc evaluate {happiness arrangement} {
    set sum 0
    foreach a [lrange $arrangement 0 end-1] b [lrange $arrangement 1 end] {
        incr sum [pairwise $happiness $a $b]
    }
    incr sum [pairwise $happiness [lindex $arrangement 0] [lindex $arrangement end]]
    return $sum
}

foreach arrangement [permutations [dict keys $happiness]] {
    set h [evaluate $happiness $arrangement]
    if {[info exists optimal]} {
        set optimal [expr {max($optimal, $h)}]
    } else {
        set optimal $h
    }
}

puts $optimal
