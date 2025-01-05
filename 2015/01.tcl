#!/usr/bin/env tclsh

set floor 0
set pos 0

proc "" {} {
    global floor pos puzzle
    switch -- $puzzle(part) {
        1 { puts $floor }
        2 { puts $pos }
    }
    exit
}

proc "(" {} {
    global floor pos
    incr floor
    incr pos
}

proc ")" {} {
    global floor pos puzzle
    incr floor -1
    incr pos
    if {$floor < 0 && $puzzle(part) == 2} {
        ""
    }
}

proc "\n" {} {
    # Ignore
}

while {1} {
    [read stdin 1]
}
