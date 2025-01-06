#!/usr/bin/env tclsh

set distances [dict create]

while {[gets stdin line] >= 0} {
    switch -matchvar match -regexp -- $line {
        {^(\w+) to (\w+) = (\d+)$} {
            lassign $match _ from to dist
        }
        default {
            error "invalid $line"
        }
    }

    dict set distances $from $to $dist
    dict set distances $to $from $dist
}

puts stderr $distances

proc pathsFrom {distances path unvisited} {
    if {$unvisited eq {}} {
        return [list $path]
    }

    set from [lindex $path end]
    set result {}

    # puts stderr "pathsFrom: path $path, from $from, unvisited $unvisited"

    foreach {to dist} [dict get $distances $from] {
        set i [lsearch -exact $unvisited $to]

        if {$i == -1} {
            # Already visited, skip
            continue
        }

        set path2 [linsert $path end $to]
        set unvisited2 [lreplace $unvisited $i $i]

        lappend result {*}[pathsFrom $distances $path2 $unvisited2]
    }

    return $result
}

proc paths distances {
    set result {}
    set unvisited [dict keys $distances]

    foreach from $unvisited {
        set i [lsearch -exact $unvisited $from]
        set unvisited2 [lreplace $unvisited $i $i]

        lappend result {*}[pathsFrom $distances $from $unvisited2]
    }

    return $result
}

proc distance {distances path} {
    set total 0

    foreach from [lrange $path 0 end-1] to [lrange $path 1 end] {
        incr total [dict get $distances $from $to]
    }

    return $total
}

switch $puzzle(part) {
    1 { set reduce tcl::mathfunc::min }
    2 { set reduce tcl::mathfunc::max }
}

puts [$reduce {*}[lmap path [paths $distances] { distance $distances $path }]]
