#!/usr/bin/env tclsh

namespace eval ::password {
    namespace export \
        fromString \
        toString
    namespace export \
        next \
        valid
}

proc ::password::fromString str {
    lreverse [split $str {}]
}

proc ::password::toString chars {
    join [lreverse $chars] {}
}

proc ::password::valid chars {
    if {[set pos [lsearch $chars \[ilo\]]] != -1} {
        return no
    }

    set straight3 no
    foreach c [lrange $chars 0 end-2] b [lrange $chars 1 end-1] a [lrange $chars 2 end] {
        set x [scan $a %c]
        set y [scan $b %c]
        set z [scan $c %c]
        if {$x + 1 == $y && $y + 1 == $z} {
            set straight3 yes
        }
    }

    set pairs [dict create]
    foreach b [lrange $chars 0 end-1] a [lrange $chars 1 end] {
        if {$a eq $b} {
            dict set pairs [list $a $b] 1
        }
    }

    if {[dict size $pairs] > 1} {
        set pairs yes
    } else {
        set pairs no
    }

    expr {$straight3 && $pairs}
}

proc ::password::incr {chars {pos 0}} {
    # FIXME: incr {z z z z}?
    while {[set char [lindex $chars $pos]] eq {z}} {
        ::incr pos
    }

    lset chars $pos [format %c [expr {[scan $char %c] + 1}]]
    if {$pos > 0} {
        set chars [lreplace $chars 0 [expr {$pos - 1}] {*}[lrepeat $pos a]]
    }

    return $chars
}

proc ::password::next chars {
    set chars [password::incr $chars]

    while {![password::valid $chars]} {
        if {[set pos [lsearch $chars \[ilo\]]] != -1} {
            set chars [password::incr $chars $pos]
        } else {
            set chars [password::incr $chars]
        }
    }

    return $chars
}

set pass [password::fromString [gets stdin]]
set pass [password::next $pass]
if {$puzzle(part) == 2} {
    set pass [password::next $pass]
}
puts [password::toString $pass]
