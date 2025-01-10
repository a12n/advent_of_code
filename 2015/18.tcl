#!/usr/bin/env tclsh

namespace eval lights {
    namespace eval position {
        namespace export add neighbours
    }
    namespace export animate lit read state update
}

proc ::lights::position::add {p u} {
    lassign $p y x
    lassign $u dy dx
    list [incr y $dy] [incr x $dx]
}

proc ::lights::position::neighbours {p} {
    set points {}
    for {set dy -1} {$dy <= 1} {incr dy} {
        for {set dx -1} {$dx <= 1} {incr dx} {
            if {$dx != 0 || $dy != 0} {
                lappend points [add $p [list $dy $dx]]
            }
        }
    }
    return $points
}

proc ::lights::animate {grid size} {
    lassign $size width height
    set next {}

    for {set y 0} {$y < $height} {incr y} {
        for {set x 0} {$x < $width} {incr x} {
            set n 0
            set p [list $y $x]

            foreach q [position::neighbours $p] {
                if {[lights::state $grid $q]} {
                    incr n
                }
            }

            switch [lights::state $grid $p] {
                on {
                    if {$n == 2 || $n == 3} {
                        dict set next $p on
                    }
                }
                off {
                    if {$n == 3} {
                        dict set next $p on
                    }
                }
            }
        }
    }

    return $next
}

proc ::lights::lit {grid} {
    dict size $grid
}

proc ::lights::read {channel} {
    set grid {}
    set y 0
    set x 0
    while {1} {
        set p [list $y $x]
        switch [::read $channel 1] {
            "#" {
                dict set grid $p on
                incr x
            }
            "." {
                # Implicit
                incr x
            }
            "\n" {
                if {[info exists width]} {
                    if {$x != $width} {
                        error "inconsistent number of columns"
                    }
                } else {
                    set width $x
                }
                set x 0
                incr y
            }
            "" {
                set height $y
                break
            }
        }
    }
    return [list $grid [list $width $height]]
}

proc ::lights::state {grid p} {
    # if {[catch { return [dict get $grid $p] }]} {
    #     return off
    # }
    if {[dict exists $grid $p]} {
        return [dict get $grid $p]
    } else {
        return off
    }
}

proc ::lights::update {grid flag args} {
    foreach p $args {
        set grid [dict replace $grid $p $flag]
    }
    return $grid
}

set steps 100
catch { set steps $env(STEPS) }

lassign [lights::read stdin] grid size
lassign $size width height

switch $puzzle(part) {
    1 {
        set alwaysOn {}
    }
    2 {
        set xMax [expr {$width - 1}]
        set yMax [expr {$height - 1}]
        set alwaysOn [list [list 0 0] [list 0 $xMax] [list $yMax 0] [list $yMax $xMax]]
    }
}
set grid [lights::update $grid on {*}$alwaysOn]

puts stderr "grid $grid size $size steps $steps"
for {set i 0} {$i < $steps} {incr i} {
    set grid [lights::update [lights::animate $grid $size] on {*}$alwaysOn]
}
puts [lights::lit $grid]
