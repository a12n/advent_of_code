#!/usr/bin/gnuplot -persist

set datafile separator ","
set grid

plot "tests/09-1/puzzle.in" with linespoints pointtype 7 pointsize 1
