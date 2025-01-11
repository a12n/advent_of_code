#!/usr/bin/env tclsh

set replacements [dict create]
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    dict update replacements $from toList {
        lappend toList $to
    }
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

proc fabricate {replacements molecule finish {seenName {}} {cacheName {}}} {
    # puts stderr "fabricate: molecule \"$molecule\""

    if {$molecule eq $finish} {
        puts stderr "fabricate: found $finish"
        return 0
    }

    if {$seenName ne {}} {
        upvar $seenName seen
    } else {
        set seen [dict create]
    }

    dict set seen $molecule yes

    if {$cacheName ne {}} {
        upvar $cacheName cache
    } else {
        set cache [dict create]
    }

    if {[dict exists $cache $molecule]} {
        puts stderr "fabricate: found \"$molecule\" in cache"
        return [dict get $cache $molecule]
    }

    set minDist {}

    dict for {from to} $replacements {
        set first 0
        set n [string length $from]

        for {set first 0} {[set first [string first $from $molecule $first]] != -1} {incr first} {
            set last [expr {$first + $n - 1}]
            set molecule2 [string replace $molecule $first $last $to]

            if {[dict exists $seen $molecule2]} {
                continue
            }

            if {[set dist [fabricate $replacements $molecule2 $finish seen cache [expr {$depth + 1}]]] ne {}} {
                incr dist
                if {$minDist eq {} || $dist < $minDist} {
                    set minDist $dist
                }
            }
        }
    }

    puts stderr "fabricate: molecule $molecule minDist $minDist"
    dict set cache $molecule $minDist

    return $minDist
}

switch $puzzle(part) {
    1 {
        puts [calibrate $replacements $molecule]
    }
    2 {
        set replacements [invert $replacements]
        puts stderr "invert $replacements"
        puts [fabricate $replacements $molecule e]
    }
}
