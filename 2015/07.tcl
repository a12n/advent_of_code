#!/usr/bin/env tclsh

proc wireSignal {wires key {cacheName {}}} {
    if {[string is digit $key]} {
        return $key
    }

    if {$cacheName ne ""} {
        upvar $cacheName cache
    } else {
        set cache [dict create]
    }

    if {![catch {dict get $cache $key} value]} {
        return $value
    }

    switch -matchvar match -regexp -- [dict get $wires $key] {
        {([&|]) (\w+) (\w+)} {
            lassign $match _ op a b
            set value [expr "[wireSignal $wires $a cache] $op [wireSignal $wires $b cache]"]
        }
        {(<<|>>) (\w+) (\d+)} {
            lassign $match _ op a n
            set value [expr "([wireSignal $wires $a cache] $op $n) & 0xFFFF"]
        }
        {~ (\w+)} {
            lassign $match _ a
            set value [expr "(~[wireSignal $wires $a cache]) & 0xFFFF"]
        }
        {(\d+)} {
            lassign $match _ value
        }
        {(\w+)} {
            lassign $match _ a
            set value [wireSignal $wires $a cache]
        }
        default {
            error "wireSignal: invalid for \"$key\""
        }
    }

    dict set cache $key $value

    return $value
}

proc parseWire {str} {
    switch -matchvar match -regexp -- $str {
        {^(\w+) (AND|LSHIFT|OR|RSHIFT) (\w+) -> (\w+)$} {
            lassign $match _ a op b q
            switch $op {
                AND { set op & }
                OR { set op | }
                LSHIFT { set op << }
                RSHIFT { set op >> }
            }
            return [list $q [list $op $a $b]]
        }
        {^NOT (\w+) -> (\w+)$} {
            lassign $match _ a q
            return [list $q [list ~ $a]]
        }
        {^(\w+) -> (\w+)$} {
            lassign $match _ a q
            return [list $q $a]
        }
        default {
            error "parseWire: invalid $str"
        }
    }
}

set wires [dict create]

while {[gets stdin line] >= 0} {
    lassign [parseWire $line] output expression
    if {[dict exists $wires $output]} {
        error "duplicate $output"
    }
    dict set wires $output $expression
}

if {$puzzle(part) == 2} {
    dict set wires b [wireSignal $wires a]
}

puts [wireSignal $wires a]
