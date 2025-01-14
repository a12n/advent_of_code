#!/usr/bin/env tclsh

set n0 20151125

proc code n {
    expr {($n * 252533) % 33554393}
}

switch -matchvar match -regexp -- [set line [read stdin]] {
    {Enter the code at row (\d+), column (\d+)\.} {
        lassign $match _ row col
        puts stderr "row $row col $col"
    }
    default {
        error "invalid input \"$line\""
    }
}
