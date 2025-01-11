#!/usr/bin/env tclsh

# N — house number
# L — at least as many present
# mI — a factor of N
#
# N = m1 * m2 * m3 * … * mN
# 10 m1 + 10 m2 + 10 m3 + … + 10 mN >= L
# 10 (m1 + m2 + m3 + … + mN) >= L
# m1 + m2 + m3 + … + mN >= L/10
#
# Find the smallest N which factors sum to at least L/10.

namespace eval ::factors {
    namespace export powers sum
}

# Prime factorization of N. Returns canonical representation as
# dictionary {p a q b r c …} such that N = p^a * q^b * r^c * ….
proc factors::powers n {
    set p 2
    set powers [dict create]
    while {$p * $p <= $n} {
        if {$n % $p == 0} {
            dict incr powers $p
            set n [expr {$n / $p}]
        } else {
            incr p
        }
    }
    if {$n != 1} {
        dict incr powers $n
    }
    return $powers
}

# https://web.archive.org/web/20180128170647/http://mathforum.org/library/drmath/view/71550.html
proc factors::sum powers {
    set prod 1
    foreach {p n} $powers {
        set sum 1
        for {set i 1} {$i <= $n} {incr i} {
            set sum [expr {$sum + $p**$i}]
        }
        set prod [expr {$prod * $sum}]
    }
    return $prod
}

set limit [expr {[gets stdin] / 10}]
for {set n 1} {1} {incr n} {
    set sum [factors::sum [factors::powers $n]]
    # puts stderr "$n: $sum $limit"
    if {$sum >= $limit} {
        break
    }
}
puts $n
