#!/usr/bin/env tclsh

proc unquote str {
    expr $str
}

proc quote str {
    string cat "\"" [string map [list "\\" "\\\\" "\"" "\\\""] $str] "\""
}

switch $puzzle(part) {
    1 { set transform unquote }
    2 { set transform quote }
}

set total 0

while {[gets stdin line] >= 0} {
    set n [string length $line]
    set m [string length [$transform $line]]
    incr total [expr {abs($n - $m)}]
}

puts $total
