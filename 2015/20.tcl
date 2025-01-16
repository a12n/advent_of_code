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

# …factors of a number N, such that:
# N = p^a * q^b * r^c
# Where, p, q and r are prime factors of the number N; a, b and c are non-negative power.
#
# Number of factors of N = (a + 1) * (b + 1) * (c + 1)
# Product of factors of N = N^(No. of factors/2)
# Sum of factors: ( p^0+p^1+...+p^a) ( q^0+ q^1+....+q^b) (r^0+r^1+...+r^c) / (p^a-1)(q^b-1)(r^c-1)

# Prime factorization of N. Returns canonical representation as
# dictionary {p a q b r c …} such that N = p^a * q^b * r^c * ….
proc factorPowers n {
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
# Formula for sum of divisors:
# https://planetmath.org/formulaforsumofdivisors
proc factorPowersSum powers {
    set prod 1
    foreach {p n} $powers {
        set sum 1
        for {set i 1} {$i <= $n} {incr i} {
            set sum [expr {$sum + $p**$i}]
        }
        set prod [expr {$prod * $sum}]
        # TODO: if needed to sum only part of factors, subtract some expressions here?
    }
    return $prod
}

proc 1 limit {
    for {set n 1} {1} {incr n} {
        set sum [factorPowersSum [factorPowers $n]]
        # puts stderr "$n: $sum $limit"
        if {$sum >= ($limit / 10)} {
            break
        }
    }
    puts $n
}

# From Sieve of Eratosthenes.
proc 2 {limit {houses 50}} {
    set sums [lrepeat [expr {2 * $limit}] 1]

    for {set n 2} {$n < $limit} {incr n} {
        lset sums $n [expr {[lindex $sums $n] + $n}]

        for {set k [expr {2 * $n}]} {$k <= ($houses * $n)} {incr k $n} {
            lset sums $k [expr {[lindex $sums $k] + $n}]
        }

        if {(11 * [lindex $sums $n]) >= $limit} {
            break
        }
    }
    puts $n
}

$puzzle(part) [expr {[gets stdin]}]
