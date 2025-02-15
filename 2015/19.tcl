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
    lassign [string map {" => " " "} $line] left right
    dict update replacements $left rightList {
        lappend rightList $right
    }
}

proc invert replacements {
    set result [dict create]
    dict for {left rightList} $replacements {
        foreach right $rightList {
            if {[dict exists $result $right]} {
                error "invert: duplicate $right"
            }
            dict set result $right $left
        }
    }
    return $result
}

set molecule [gets stdin]

proc calibrate {replacements molecule} {
    set replaced [dict create]

    dict for {left rightList} $replacements {
        set n [string length $left]
        set first 0
        while {[set first [string first $left $molecule $first]] != -1} {
            set last [expr {$first + $n - 1}]
            foreach right $rightList {
                dict set replaced [string replace $molecule $first $last $right] yes
            }
            incr first
        }
    }

    puts stderr "replaced [lsort -index 0 -stride 2 $replaced] [dict size $replaced]"

    return [dict size $replaced]
}

proc fabricate {anchorRules otherRules molecule finish} {
    set nSolutions [llength [dict values $otherRules $finish]]

    puts stderr "fabricate: finish \"$finish\" nSolutions $nSolutions"

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
            if {[incr nSolutions -1] == 0} {
                break
            } else {
                continue
            }
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

proc partitionRules {rules beginToken endToken} {
    set anchorRules [dict create]
    set otherRules [dict create]

    dict for {right left} $rules {
        if {[set first [string first $beginToken $right]] != -1 && [string first $endToken $right $first] != -1} {
            dict set anchorRules $right $left
        } else {
            dict set otherRules $right $left
        }
    }
    puts stderr "partitionRules: rules $rules"
    puts stderr "partitionRules: anchorRules $anchorRules"
    puts stderr "partitionRules: otherRules $otherRules"

    return [list $anchorRules $otherRules]
}

switch $puzzle(part) {
    1 {
        puts [calibrate $replacements $molecule]
    }
    2 {
        # FIXME: Rn/Ar are input specific.
        lassign [partitionRules [invert $replacements] "Rn" "Ar"] anchorRules otherRules
        puts [fabricate $anchorRules $otherRules $molecule e]
    }
}
