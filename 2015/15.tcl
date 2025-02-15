#!/usr/bin/env tclsh

proc multipliy {nums ingredients} {
    set n [llength $ingredients]
    set cookie {
        calories 0
        capacity 0
        durability 0
        flavor 0
        texture 0
    }

    for {set i 0} {$i < 5} {incr i} {
        # Index of property with stride 2.
        set i2 [expr {2 * $i + 1}]

        for {set j 0} {$j < $n} {incr j} {
            set x [lindex $nums $j]
            set a [lindex [lindex $ingredients $j] $i2]
            lset cookie $i2 [expr {[lindex $cookie $i2] + $x * $a}]
        }
    }

    return $cookie
}

proc score cookie {
    set prod 1
    foreach {key value} $cookie {
        if {$key != {calories}} {
            set prod [expr {$prod * max(0, $value)}]
        }
    }
    return $prod
}

proc caloriesPenalty {cookie {optimal 500}} {
    expr {abs($optimal - [dict get $cookie calories])}
}

proc initial n {
    set k [expr {100 / $n}]
    set m [expr {100 % $n}]
    list {*}[lrepeat [expr {$n - 1}] $k] [expr {$k + $m}]
}

proc increment nums {
    set n [llength $nums]
    set pos 0
    while {[set value [lindex $nums $pos]] == 100} {
        lset nums $pos 0
        incr pos
    }
    lset nums $pos [incr value]
    return $nums
}

# Convert normalized "barycentric" vector [x1 x2 … xN-1] to
# [x1 x2 … xN-1 xN].
proc unnormalize nums {
    set last 100
    foreach x $nums {
        if {[set last [expr {$last - $x}]] < 0} {
            error "invalid coefficients"
        }
    }
    lreplace $nums end+1 end+1 $last
}

proc neighbors nums {
    set n [llength $nums]
    set result {}
    for {set i 0} {$i < $n} {incr i} {
        if {[set atI [expr {[lindex $nums $i] + 1}]] > 100} {
            continue
        }

        for {set j 0} {$j < $n} {incr j} {
            if {$i == $j} {
                continue
            }

            if {[set atJ [expr {[lindex $nums $j] - 1}]] < 0} {
                continue
            }

            lappend result [lreplace [lreplace $nums $i $i $atI] $j $j $atJ]
        }
    }
    return $result
}

proc localSearch {ingredients nums} {
    set cache [dict create]
    set queue [list $nums]

    puts stderr "localSearch: initial $nums"

    while {$queue ne {}} {
        set queue [lassign $queue elt]

        puts stderr "localSearch: elt $elt queue $queue"

        if {[catch {dict get $cache $elt} value]} {
            set value [score [multipliy $elt $ingredients]]
            dict set cache $elt $value
        }

        puts stderr "localSearch: value $value"

        if {[info exists bestScore] && $value < $bestScore} {
            puts stderr "localSearch: bestScore $bestScore"
            continue
        }

        set best $elt
        set bestScore $value

        foreach move [neighbors $elt] {
            if {![dict exists $cache $move]} {
                puts stderr "localSearch: move $move"
                lappend queue $move
            }
        }
    }

    return $bestScore
}

# List of cookies from the specified ingredients with 500 calories each.
proc caloriesCookies ingredients {
    set cookies {}
    set n [llength $ingredients]
    set nums0 [lrepeat [expr {$n - 1}] 0]

    while {1} {
        if {![catch { set nums [unnormalize $nums0] }]} {
            set cookie [multipliy $nums $ingredients]

            if {[caloriesPenalty $cookie] == 0} {
                lappend cookies $cookie
            }
        }

        if {[catch { set nums0 [increment $nums0] }]} {
            break
        }
    }

    return $cookies
}

proc 1 ingredients {
    # Local search from arbitrary initial solution.
    puts [localSearch $ingredients [initial [llength $ingredients]]]
}

proc 2 ingredients {
    puts [tcl::mathfunc::max {*}[lmap cookie [caloriesCookies $ingredients] {
        score $cookie
    }]]
}

set ingredients {}
while {[gets stdin line] >= 0} {
    set ingredient [lrange [string map {, {} : {}} $line] 1 end]
    lappend ingredients [lsort -index 0 -stride 2 $ingredient]
}

$puzzle(part) $ingredients

# N = number of ingredients
#
# x = [x1 x2 … xN]
# A = [
# capacity1 durability1 flavor1 texture1;
# capacity2 durability2 flavor2 texture2;
# …
# capacityN durabilityN flavorN textureN
# ]
# x A = B = [capacity durability flavor texture]
#
# Σ x = 100
# max Π B

# Maximize the following:
# (x1 * c1 + x2 * c2 + x3 * c3 + … + xN * cN) *
# (x1 * d1 + x2 * d2 + x3 * d3 + … + xN * dN) *
# (x1 * f1 + x2 * f2 + x3 * f3 + … + xN * fN) *
# (x1 * t1 + x2 * t2 + x3 * t3 + … + xN * tN)

# Plot example in Gnuplot:
# max0(x) = (x + abs(x)) / 2
# y(x) = 100 - x
# plot [0:100] max0(-1 * x + 2 * y(x)) * max0(-2 * x + 3 * y(x)) * max0(6 * x - 2 * y(x)) * max0(3 * x - 1 * y(x))

# Local search from an initial solution?
