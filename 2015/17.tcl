#!/usr/bin/env tclsh

set liters 150
catch { set liters $env(LITRES) }
puts stderr "liters $liters"

set capacities [lsort -decreasing -integer [read stdin]]
puts stderr "capacities $capacities"
