#!/usr/bin/env tclsh

proc normalize record {
    lsort -index 0 -stride 2 $record
}

proc unifies {reference record} {
    foreach {key value} $record {
        if {[dict get $reference $key] > $value} {
            return no
        }
    }
    return yes
}

proc distance {reference record} {
    set dist 0
    foreach {key value} $record {
        incr dist [expr {abs([dict get $reference $key] - $value)}]
    }
    return $dist
}

set sample \
    [normalize {
        akitas 0
        cars 2
        cats 7
        children 3
        goldfish 5
        perfumes 1
        pomeranians 3
        samoyeds 2
        trees 3
        vizslas 0
    }]

puts stderr "sample $sample"

set notes {}
while {[gets stdin line] >= 0} {
    # TODO: don't build the list?
    lappend notes [normalize [string map { , {} : {} } $line]]
}

foreach note $notes {
    set id [dict get $note Sue]
    set record [dict remove $note Sue]

    if {![unifies $sample $record]} {
        puts stderr "$id: record $record not unifies"
        continue
    }

    set dist [distance $sample $record]

    puts stderr "$id: record $record distance $dist"

    if {[info exists leastDistance]} {
        if {$dist < $leastDistance} {
            set idBest $id
            set leastDistance $dist
        } elseif {$dist == $leastDistance} {
            lappend idBest $id
        }
    } else {
        set idBest $id
        set leastDistance $dist
    }
}

puts stderr "leastDistance $leastDistance"
puts $idBest
