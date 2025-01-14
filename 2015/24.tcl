#!/usr/bin/env tclsh

set weights [lsort -integer [read stdin]]
set totalWeight [tcl::mathop::+ {*}$weights]
puts stderr "weights $weights totalWeight $totalWeight"

proc searchGroup {weights target} {
    set queue [list [list 0 1 {} $weights]]
    set solutions 0

    while {$queue ne {}} {
        puts stderr "searchGroup: queue [llength $queue]"

        # set queue [lassign $queue state]

        set state [lindex $queue end]
        set queue [lreplace $queue end end]

        lassign $state sum prod group weights

        puts stderr "searchGroup: sum $sum prod $prod group {$group} weights {$weights}"

        if {$sum > $target} {
            # The state and all it's childern can't be a solution.
            puts stderr "searchGroup: sum $sum > target $target"
            continue
        }

        if {[info exists bestGroup]} {
            if {[llength $group] > [llength $bestGroup]} {
                # Already worse than known smaller group.
                puts stderr "searchGroup: group {$group} already longer than best {$bestGroup}"
                continue
            }
            if {$prod > $bestGroupProd} {
                # Already worse than known group with smaller QE.
                puts stderr "searchGroup: QE $prod already larger than best $bestGroupProd"
                continue
            }
        }

        if {$sum == $target} {
            # Found a solution state, and it's better than the
            # previous one (due to bestGroup checks above).
            puts stderr "searchGroup: found solution"
            set bestGroup $group
            set bestGroupProd $prod
            set leftoverWeights $weights
            if {[incr solutions] >= 5} {
                break
            } else {
                continue
            }
        }

        # Try to select each possible package into the group.
        for {set i 0} {$i < [llength $weights]} {incr i} {
            set weight [lindex $weights $i]
            set weights2 [lreplace $weights $i $i]
            set group2 [lreplace $group end+1 end+1 $weight]

            lappend queue [list [expr {$sum + $weight}] [expr {$prod * $weight}] $group2 $weights2]
        }
    }

    if {[info exists bestGroup]} {
        return [list $bestGroup $bestGroupProd $leftoverWeights]
    } else {
        error "searchGroup: infeasible target $target"
    }
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

switch $puzzle(part) {
    1 { set n 3 }
    2 { set n 4 }
}

if {$totalWeight % $n != 0} {
    error "invalid weights $weights"
}

lassign [searchGroup $weights [expr {$totalWeight / $n}]] first firstQE weights
puts stderr "first $first firstSum [tcl::mathop::+ {*}$first] firstQE $firstQE"
lassign [searchGroup $weights [expr {$totalWeight / $n}]] second _ weights
puts stderr "second $second secondSum [tcl::mathop::+ {*}$second]"
switch $n {
    3 {
        puts stderr "third $weights thirdSum [tcl::mathop::+ {*}$weights]"
    }
    4 {
        lassign [searchGroup $weights [expr {$totalWeight / $n}]] third _ fourth
        puts stderr "third $third thirdSum [tcl::mathop::+ {*}$third]"
        puts stderr "fourth $fourth fourthSum [tcl::mathop::+ {*}$fourth]"
    }
}
puts $firstQE
