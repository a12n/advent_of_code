#!/usr/bin/env tclsh

namespace eval ::properties {
    # Part 1
    namespace export \
        aa \
        abcdpqxy \
        aeiou

    # Part 2
    namespace export \
        aba \
        abab
}

# Contains at least one letter that appears twice in a row.
proc ::properties::aa chars {
    foreach a [lrange $chars 0 end-1] b [lrange $chars 1 end] {
        if {$a == $b} {
            return yes
        }
    }
    return no
}

# Does not contain the strings ab, cd, pq, or xy.
proc ::properties::abcdpqxy chars {
    foreach a [lrange $chars 0 end-1] b [lrange $chars 1 end] {
        if {"$a$b" in {ab cd pq xy}} {
            return no
        }
    }
    return yes
}

# Contains at least three vowels (aeiou only).
proc ::properties::aeiou chars {
    expr {[llength [lsearch -all -glob -inline $chars \[aeiou\]]] > 2}
}

# Contains at least one letter which repeats with exactly one letter
# between them.
proc ::properties::aba chars {
    foreach a [lrange $chars 0 end-2] b [lrange $chars 2 end] {
        if {$a == $b} {
            return yes
        }
    }
    return no
}

# Contains a pair of any two letters that appears at least twice in
# the string without overlapping.
proc ::properties::abab chars {
    set first [dict create]
    set pos 0

    foreach a [lrange $chars 0 end-1] b [lrange $chars 1 end] {
        if {[dict exists $first "$a$b"]} {
            if {($pos - [dict get $first "$a$b"]) > 1} {
                return yes
            }
        } else {
            dict set first "$a$b" $pos
        }

        incr pos
    }

    return no
}

switch $puzzle(part) {
    1 {
        proc nice chars {
            expr {[properties::aeiou $chars] && [properties::aa $chars] && [properties::abcdpqxy $chars]}
        }
    }
    2 {
        proc nice chars {
            expr {[properties::abab $chars] && [properties::aba $chars]}
        }
    }
}

set n 0

while {[gets stdin line] >= 0} {
    if {[nice [split $line ""]]} {
        incr n
    }
}

puts $n
