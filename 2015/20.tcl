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

# Finding the Sum of the Factors of a Number:
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

proc 1 limit {
    for {set n 1} {1} {incr n} {
        set sum [factors::sum [factors::powers $n]]
        # puts stderr "$n: $sum $limit"
        if {$sum >= ($limit / 10)} {
            break
        }
    }
    puts $n
}

proc 2 limit {
    puts [factors::powers 8]
    # 2 3
    # 2^0 = 1
    # 2^1 = 2
    # 2^2 = 4
    # 2^3 = 8
    # 8+4+2+1=15
    # If elfs visit only 5 houses:
    # 8+4+0+0=12
    exit
    set table [dict create]
    for {set e 1} {$e <= 100} {incr e} {
        for {set n 1} {$n <= 50} {incr n} {
            dict incr table [expr {$e * $n}] $e
        }
    }
    foreach {k v} $table {
        puts "$k: $v $limit"
    }
}

$puzzle(part) [expr {[gets stdin]}]
