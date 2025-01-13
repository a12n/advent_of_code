#!/usr/bin/env tclsh

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
    set result {}
    dict for {from toList} $replacements {
        foreach to $toList {
            lappend result $to $from
        }
    }
    return [lsort -decreasing -command {apply {{a b} { expr {[string length $a] - [string length $b]} }}} -stride 2 $result]
}

set molecule [gets stdin]

puts stderr "replacements $replacements [dict size $replacements], molecule $molecule [string length $molecule]"

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

    set queue [list [list 0 $molecule]]
    set seen [dict create]

    while {$queue ne {}} {
        puts stderr "fabricate: queue [llength $queue] seen [dict size $seen]"

        set state [lindex $queue end]
        set queue [lreplace $queue end end]

        lassign $state dist molecule

        puts stderr "fabricate: $dist \"$molecule\""

        if {$molecule eq $finish} {
            if {[info exists minDist]} {
                set minDist [expr {min($dist, $minDist)}]
            } else {
                set minDist $dist
            }
            continue
        }

        dict set seen $molecule yes

        set dist2 [expr {$dist + 1}]

        dict for {to from} $replacements {
            set first 0
            set n [string length $to]

            for {set first 0} {[set first [string first $to $molecule $first]] != -1} {incr first} {
                set last [expr {$first + $n - 1}]
                set molecule2 [string replace $molecule $first $last $from]

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

switch $puzzle(part) {
    1 {
        puts [calibrate $replacements $molecule]
    }
    2 {
        set replacements [invert $replacements]
        puts stderr "invert $replacements"
        set molecule [unrecur2 replacements $molecule]
        puts [fabricate $replacements $molecule e]
    }
}
