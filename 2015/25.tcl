#!/usr/bin/env tclsh

set x0 20151125
proc next {x {n 1}} {
    # TODO: ^n?
    expr {($x * 252533) % 33554393}
}

# Convert (r,c) coordinate to 0-based index in the sequence.
#
#    | 0   1   2   3   4   5   6
# ---+---+---+---+---+---+---+---
#  0 |  0   2   5   9  14  20  27
#  1 |  1   4   8  13  19  26
#  2 |  3   7  12  18  25
#  3 |  6  11  17  24
#  4 | 10  16  23
#  5 | 15  22
#  6 | 21
#
# The (r,c) element is at +c from (r+c,0) element.
# E.g., element (2,3) is at +3 from (5,0) element.
#
# The (r,0) element is at +r from (r-1,0) element.
# E.g., element (5,0) is at +5 from (4,0) element.
#
# The (0,0) element is at 0.
#
# So, the (5,0) is at:
# 5+(4,0)
# 5+4+(3,0)
# 5+4+3+(2,0)
# 5+4+3+2+(1,0)
# 5+4+3+2+1+(0,0)
# 5+4+3+2+1+0
#
# So, the (r,0) element is at (r^2 + r)/2 (triangular number).
# E.g., element (5,0) is at (5^2 + 5)/2 = 15.
#
# So, (r,c) element is at (r+c,0)+c = ((r+c)^2 + (r+c))/2 + c.
# E.g., element (2,3) is at:
# ((2+3)^2 + (2+3))/2 + 3 =
# (25 + 5)/2 + 3 =
# 15 + 3 =
# 18
proc index {row col} {
    expr {(($row + $col)**2 + $row + $col) / 2 + $col}
}

switch -matchvar match -regexp -- [set line [read stdin]] {
    {Enter the code at row (\d+), column (\d+)\.} {
        lassign $match _ row col
        # Make them 0-based offsets
        incr row -1
        incr col -1
    }
    default {
        error "invalid input \"$line\""
    }
}

set x $x0
for {set n [index $row $col]} {$n > 0} {incr n -1} {
    set x [next $x]
}
puts $x
