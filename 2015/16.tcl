#!/usr/bin/env tclsh

proc normalize record {
    lsort -index 0 -stride 2 $record
}

proc unifyExact {reference record} {
    foreach {key value} $record {
        set refValue [dict get $reference $key]
        if {$value != $refValue} {
            return no
        }
    }
    return yes
}

proc unifyRange {reference record} {
    foreach {key value} $record {
        set refValue [dict get $reference $key]

        if {$key in {cats trees} && $value <= $refValue} {
            return no
        } elseif {$key in {goldfish pomeranians} && $value >= $refValue} {
            return no
        # } elseif {$value != $refValue} {
        #     return no
        }
    }
    return yes
}

proc unifies {reference record} {
    foreach {key value} $record {
        if {[dict get $reference $key] > $value} {
            return no
        }
    }
    return yes
}

proc distanceRange {reference record {ignored {cats goldfish pomeranians trees}}} {
    set dist 0
    foreach {key value} $record {
        if {$key in $ignored} {
            continue
        }
        incr dist [expr {abs([dict get $reference $key] - $value)}]
    }
    return $dist
}

proc distanceExact {reference record} {
    distanceRange $reference $record {}
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

switch $puzzle(part) {
    1 {
        set distance distanceExact
        set unify unifyExact
    }
    2 {
        set distance distanceRange
        set unify unifyRange
    }
}

foreach note $notes {
    set id [dict get $note Sue]
    set record [dict remove $note Sue]

    if {![$unify $sample $record]} {
        puts stderr "$id: record $record: not unify"
        continue
    }

    set dist [$distance $sample $record]

    puts stderr "$id: record $record: distance $dist"

    if {[info exists leastDistance]} {
        if {$dist < $leastDistance} {
            set idBest $id
            set leastDistance $dist
            puts stderr "$id: new leastDistance $leastDistance"
        } elseif {$dist == $leastDistance} {
            lappend idBest $id
            puts stderr "$id: same leastDistance $leastDistance"
        }
    } else {
        set idBest $id
        set leastDistance $dist
        puts stderr "$id: initial leastDistance $leastDistance"
    }
}

puts stderr "leastDistance $leastDistance"
puts $idBest
