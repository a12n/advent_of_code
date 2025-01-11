#!/usr/bin/env tclsh

set program {}
while {[gets stdin line] >= 0} {
    lappend program [string map {, {}} $line]
}

set cpu [dict create a 0 b 0]
puts stderr "cpu $cpu"
for {set pc 0} {$pc < [llength $program]} {} {
    lassign [lindex $program $pc] opcode fst snd
    puts stderr "$pc: $opcode $fst $snd"
    switch -- $opcode {
        hlf {
            dict update cpu $fst n {
                set n [expr {$n / 2}]
            }
            incr pc
        }
        tpl {
            dict update cpu $fst n {
                set n [expr {$n * 3}]
            }
            incr pc
        }
        inc {
            dict incr cpu $fst
            incr pc
        }
        jmp {
            incr pc $fst
        }
        jie {
            if {[dict get $cpu $fst] % 2 == 0} {
                incr pc $snd
            } else {
                incr pc
            }
        }
        jio {
            if {[dict get $cpu $fst] == 1} {
                incr pc $snd
            } else {
                incr pc
            }
        }
        default {
            error "invalid instruction $opcode"
        }
    }
    puts stderr "$cpu"
}

puts [dict get $cpu b]
