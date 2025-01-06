#!/usr/bin/env tclsh

set distances [dict create]

while {[gets stdin line] >= 0} {
    switch -matchvar match -regexp -- $line {
        {^(\w+) to (\w+) = (\d+)$} {
            lassign $match _ from to dist
        }
        default {
            error "invalid $line"
        }
    }

    dict set distances $from $to $dist
    dict set distances $to $from $dist
}

puts stderr $distances

# TODO
