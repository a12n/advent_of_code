#!/usr/bin/env tclsh

set weights [lsort -decreasing -integer [read stdin]]

set totalWeight [tcl::mathop::+ {*}$weights]
set groupWeight [expr {$totalWeight / 3}]
if {$totalWeight % 3 != 0} {
    error "invalid weights $weights"
}

puts stderr "weights $weights totalWeight $totalWeight groupWeight $groupWeight"
