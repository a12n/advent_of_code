#!/usr/bin/env tclsh

proc tokens str {
    set last 0
    set result {}
    for {set i 0} {$i < [string length $str]} {incr i} {
        if {[string is upper [string index $str $i]]} {
            if {$i > $last} {
                lappend result [string range $str $last [expr {$i - 1}]]
            }
            set last $i
        }
    }
    if {$i > $last} {
        lappend result [string range $str $last [expr {$i - 1}]]
    }
    return $result
}

set replacements [dict create]
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    dict update replacements $from toList {
        lappend toList $to
    }
}

proc recursive {from to} {
    expr {[string first $from $to] != -1}
}

proc recursive2 {from to} {
    expr {"$to" eq "$from$from"}
}

proc invert replacements {
    set result [dict create]
    dict for {from toList} $replacements {
        foreach to $toList {
            if {[dict exists $result $to]} {
                error "invert: duplicate $to"
            }
            dict set result $to $from
        }
    }
    return $result
}

set molecule [gets stdin]

# puts stderr "replacements $replacements [dict size $replacements], molecule $molecule [string length $molecule]"

proc calibrate {replacements molecule} {
    set replaced [dict create]

    dict for {from toList} $replacements {
        set n [string length $from]
        set first 0
        while {[set first [string first $from $molecule $first]] != -1} {
            set last [expr {$first + $n - 1}]
            foreach to $toList {
                dict set replaced [string replace $molecule $first $last $to] yes
            }
            incr first
        }
    }

    puts stderr "replaced [lsort -index 0 -stride 2 $replaced] [dict size $replaced]"

    return [dict size $replaced]
}

proc unrecur2 {replacementsName molecule} {
    upvar $replacementsName replacements

    puts stderr "unrecur2: replacements [dict size $replacements] molecule [string length $molecule]"

    dict for {to from} $replacements {
        if {![recursive2 $from $to]} {
            continue
        }

        set first 0
        set n [string length $to]

        for {set first 0} {[set first [string first $to $molecule $first]] != -1} {incr first} {
            set last [expr {$first + $n - 1}]
            puts stderr "unrecur: replace \"$to\" with \"$from\""
            puts stderr $molecule
            set molecule [string replace $molecule $first $last $from]
            puts stderr $molecule
        }

        dict unset replacements $to
    }

    puts stderr "unrecur2: replacements [dict size $replacements] molecule [string length $molecule]"

    return $molecule
}

proc fabricate {replacements molecule finish} {
    puts stderr "fabricate: replacements [dict size $replacements] molecule [string length $molecule] finish \"$finish\""

    set anchorRules [dict create]
    set otherRules [dict create]

    dict for {right left} $replacements {
        if {[string first "Rn" $right] != -1 && [string first "Ar" $right] != -1} {
            dict set anchorRules $right $left
        } else {
            dict set otherRules $right $left
        }
    }
    puts stderr "fabricate: replacements $replacements"
    puts stderr "fabricate: anchorRules $anchorRules"
    puts stderr "fabricate: otherRules $otherRules"

    set queue [list [list 0 $molecule]]
    set seen [dict create]

    while {$queue ne {}} {
        puts stderr "fabricate: queue [llength $queue] seen [dict size $seen]"

        set state [lindex $queue end]
        set queue [lreplace $queue end end]

        lassign $state dist molecule

        puts stderr "fabricate: $dist \"$molecule\""

        if {[info exists minDist]} {
            if {$dist > $minDist} {
                puts stderr "fabricate: $dist already greater than $minDist"
                continue
            }
        }

        if {$molecule eq $finish} {
            puts stderr "fabricate: found \"$finish\" after $dist replacements"
            set minDist $dist
            continue
        }

        dict set seen $molecule yes

        # Replace all Rn/Ar first.
        dict for {right left} $anchorRules {
            set first 0
            set n [string length $right]

            for {set first 0} {[set first [string first $right $molecule $first]] != -1} {incr first} {
                puts stderr "fabricate: \"$right\" -> \"$left\""
                set last [expr {$first + $n - 1}]
                set molecule2 [string replace $molecule $first $last $left]
                set molecule $molecule2
                incr dist
            }
        }

        set dist2 [expr {$dist + 1}]

        dict for {right left} $otherRules {
            set first 0
            set n [string length $right]

            for {set first 0} {[set first [string first $right $molecule $first]] != -1} {incr first} {
                set last [expr {$first + $n - 1}]
                set molecule2 [string replace $molecule $first $last $left]

                if {[dict exists $seen $molecule2]} {
                    continue
                }

                lappend queue [list $dist2 $molecule2]
            }
        }
    }

    if {[info exists minDist]} {
        return $minDist
    } else {
        error "fabricate: infeasible \"$finish\""
    }
}

proc printNotCovered {rulesInv molecule} {
    set covered [dict create]

    dict for {from to} $rulesInv {
        set first 0
        set n [string length $from]

        for {set first 0} {[set first [string first $from $molecule $first]] != -1} {incr first} {
            for {set last [expr {$first + $n - 1}]} {$last >= $first} {incr last -1} {
                dict set covered $last yes
            }
        }
    }

    for {set i 0} {$i < [string length $molecule]} {incr i} {
        if {[dict exists $covered $i]} {
            puts -nonewline stderr [string index $molecule $i]
        } else {
            puts -nonewline stderr "\x1b\[31m[string index $molecule $i]\x1b\[0m"
        }
    }
    puts stderr "\n"
}

proc printRnAr molecule {
    set n 0
    foreach t [tokens $molecule] {
        if {$t == "Rn"} {
            puts -nonewline stderr "\x1b\[32m$t"
            incr n
        } elseif {$t == "Ar"} {
            puts -nonewline stderr "\x1b\[31m$t"
            incr n -1
        } else {
            puts -nonewline stderr "$t"
        }
        puts -nonewline stderr "\x1b\[0m"
    }
    puts stderr ""
    puts stderr "Rn/Ar balance $n"
}

proc replaceRnAr {rules molecule what} {
    dict for {from to} $rules {
        if {$what eq {rnar}} {
            if {[string first "Rn" $from] == -1 || [string first "Ar" $from] == -1} {
                # puts stderr "replaceRnAr: ignore non-Rn/Ar rule \"$from\" \"$to\""
                continue
            }
        } elseif {$what eq {recursive}} {
            if {![recursive $to $from]} {
                # puts stderr "replaceRnAr: ignore non-recursive rule \"$from\" \"$to\""
                continue
            }
        } elseif {$what eq {notrnar}} {
            if {[string first "Rn" $from] != -1 && [string first "Ar" $from] != -1} {
                # puts stderr "replaceRnAr: ignore Rn/Ar rule \"$from\" \"$to\""
                continue
            }
        }

        # puts stderr "replaceRnAr: trying to replace \"$from\" with \"$to\""

        set first 0
        set n [string length $from]

        for {set first 0} {[set first [string first $from $molecule $first]] != -1} {incr first} {
            set last [expr {$first + $n - 1}]
            set molecule2 [string replace $molecule $first $last $to]
            # puts stderr "$molecule -> $molecule2"
            set molecule $molecule2
        }
    }
    return $molecule
}

proc printFreqs molecule {
    set freqs [dict create]
    foreach t [tokens $molecule] {
        dict incr freqs $t
    }
    puts stderr "Token frequencies: $freqs"
    puts stderr "Tokens with the same frequency:"
    foreach t [dict keys $freqs] {
        foreach s [dict keys $freqs] {
            if {$t >= $s} {
                continue
            }
            if {[set n [dict get $freqs $t]] == [dict get $freqs $s]} {
                puts stderr "$n $t $s"
            }
        }
    }
    puts stderr ""
}

switch $puzzle(part) {
    1 {
        puts [calibrate $replacements $molecule]
    }
    2 {
        puts [fabricate [invert $replacements] $molecule e]
    }
}
