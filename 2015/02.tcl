#!/usr/bin/env tclsh

proc wrappingArea {l w h} {
    set lw [expr {$l * $w}]
    set wh [expr {$w * $h}]
    set hl [expr {$h * $l}]
    expr {2 * $lw + 2 * $wh + 2 * $hl + min($lw, $wh, $hl)}
}

proc ribbonLength {l w h} {
    set p1 [expr {2 * ($l + $w)}]
    set p2 [expr {2 * ($w + $h)}]
    set p3 [expr {2 * ($h + $l)}]
    expr {min($p1, $p2, $p3) + $l * $w * $h}
}

switch -- $puzzle(part) {
    1 { set property wrappingArea }
    2 { set property ribbonLength }
}

set total 0

while {[gets stdin line] > 0} {
    incr total [$property {*}[split $line x]]
}

puts $total
