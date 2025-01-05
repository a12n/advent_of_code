#!/usr/bin/env tclsh

switch $puzzle(part) {
    1 { set modulus 1 }
    2 { set modulus 2 }
}

array set x {
    0 0
    1 0
}
array set y {
    0 0
    1 0
}

array set visited {}

for {set i 0} {1} {incr i} {
    set j [expr {$i % $modulus}]

    incr visited([set pos "$x($j) $y($j)"])

    switch -- [read stdin 1] {
        "<" { incr x($j) -1 }
        ">" { incr x($j) }
        "^" { incr y($j) -1 }
        "v" { incr y($j) }
        "\n" { continue }
        "" { break }
        default { error "invalid instruction" }
    }
}

puts [array size visited]
