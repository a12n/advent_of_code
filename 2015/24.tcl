#!/usr/bin/env tclsh

set weights [lsort -decreasing -integer [read stdin]]
set totalWeight [tcl::mathop::+ {*}$weights]
set groupWeight [expr {$totalWeight / 3}]
if {$totalWeight % 3 != 0} {
    error "invalid weights $weights"
}

# Greedy first group for minimal number of weights.
proc firstGroup {weights groupWeight {group {}} {accum 0}} {
    if {$accum == $groupWeight} {
        return [list $group $weights]
    } elseif {$accum > $groupWeight} {
        return {}
    }

    for {set i 0} {$i < [llength $weights]} {incr i} {
        set weight [lindex $weights $i]
        set result [firstGroup [lreplace $weights $i $i] $groupWeight [lreplace $group end+1 end+1 $weight] [expr {$accum + $weight}]]
        if {$result ne {}} {
            return $result
        }
    }

    return {}
}

puts stderr "weights $weights totalWeight $totalWeight groupWeight $groupWeight"

lassign [firstGroup $weights $groupWeight] first weights
puts stderr "first $first weights $weights"
lassign [firstGroup $weights $groupWeight] second weights
puts stderr "second $second weights $weights"

puts [tcl::mathop::* {*}$first]
