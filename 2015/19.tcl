#!/usr/bin/env tclsh

set replacements [dict create]
while {[gets stdin line] > 0} {
    lassign [string map {" => " " "} $line] from to
    dict update replacements $from toList {
        lappend toList $to
    }
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

proc fabricate {replacements start finish} {
    set queue [list [list 0 $start]]
    set seen [dict create]

    while {$queue ne {}} {
        set queue [lassign $queue elt]
        lassign $elt dist molecule

        puts stderr "fabricate: dist $dist molecule \"$molecule\" queue [llength $queue]"

        # TODO: keep only "changes" in the queue, not the whole strings?
        # TODO: A* with some edit distance heuristic?
        if {$molecule eq $finish} {
            return $dist
        }

        dict for {from toList} $replacements {
            set n [string length $from]
            set first 0
            while {[set first [string first $from $molecule $first]] != -1} {
                set last [expr {$first + $n - 1}]
                foreach to $toList {
                    set dist2 [expr {$dist + 1}]
                    set molecule2 [string replace $molecule $first $last $to]
                    if {![dict exists $seen $molecule2]} {
                        lappend queue [list $dist2 $molecule2]
                        dict set seen $molecule2 yes
                    }
                }
                incr first
            }
        }

        set queue [lsort -index 0 -integer $queue]
    }

    error "fabricate: no path from $start to $finish"
}

switch $puzzle(part) {
    1 { puts [calibrate $replacements $molecule] }
    2 { puts [fabricate $replacements e $molecule] }
}
