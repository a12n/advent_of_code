#!/usr/bin/env tclsh

switch $puzzle(part) {
    1 {
        proc turnOff light { expr 0 }
        proc turnOn light { expr 1 }
        proc toggle light { expr {!$light} }
    }
    2 {
        proc turnOff light { expr {max(0, $light - 1)} }
        proc turnOn light { expr {$light + 1} }
        proc toggle light { expr {$light + 2} }
    }
}

set nRows 1000
set nCols 1000

# TODO: 2D extents of lights, add/subtract new regions
set lights [lrepeat [expr {$nRows * $nCols}] 0]

proc index {row col} {
    global nRows nCols
    expr {$row * $nCols + $col}
}

while {[gets stdin line] >= 0} {
    lassign [string map {
        " on" "On"
        " off" "Off"
        " through" ""
        "," " "
    } $line] cmd x1 y1 x2 y2

    set minX [expr {min($x1, $x2)}]
    set minY [expr {min($y1, $y2)}]
    set maxX [expr {max($x1, $x2)}]
    set maxY [expr {max($y1, $y2)}]

    puts stderr "$cmd $x1 $y1 $x2 $y2"
    for {set x $minX} {$x <= $maxX} {incr x} {
        for {set y $minY} {$y <= $maxY} {incr y} {
            set i [index $y $x]
            lset lights $i [$cmd [lindex $lights $i]]
        }
    }
}

puts [tcl::mathop::+ {*}$lights]
