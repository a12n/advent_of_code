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

proc 2 limit {
    puts [factors::powers 8]
    # 2 3
    # 2^0 = 1
    # 2^1 = 2
    # 2^2 = 4
    # 2^3 = 8
    # 8+4+2+1=15
    #
    # If elves visit only 5 houses:
    # 2^0 = 1 no
    # 2^1 = 2
    # 2^2 = 4
    # 2^3 = 8
    # 8+4+2+0=14
    #
    # Elves visit only N=5 next houses
    #
    # Elf E=2 visits houses 2 4 6 8 10
    # At house H=8, sum factors up to (H=8 - E=2 * N=5) = -2 backward.
    # At house H=10, sum factors up to (H=10 - E=2 * N=5) = 0 backward.
    # At house H=12 (first house of E=2 which he doesn't visit), sum factors up to (H=12 - E=2 * N=5) = 2 backward.
    #
    # Elf E=3 visits houses 3 6 9 12 15
    # At house H=12 elves E={12 6 4 3} deliver and E={2 1} don't
    # 12+6+4+3=25
    #
    # At house H only the biggest N=5-1 dividers deliver?
    #
    # At house=20, dividers [20 10 5 4 2 1]. So, only dividers [20 10 5 4] deliver?
    # So, the sum will be 20+10+5+4+2=39?
    set table [dict create]
    for {set e 1} {$e <= 25} {incr e} {
        for {set n 1} {$n <= 5} {incr n} {
            set k [expr {$e * $n}]
            dict incr table $k $e
            puts "elf $e delivers $e to $k = [dict get $table $k]"
        }
    }
    foreach {k v} [lsort -stride 2 -index 0 -integer $table] {
        puts "$k: $v [expr {$limit / 11}]"
    }
}

$puzzle(part) [expr {[gets stdin]}]
