#!/usr/bin/env tclsh

namespace eval ::json {
    variable numAccum 0
    variable numSign 1

    variable strAccum ""
    variable strEscape no

    namespace export tokens
}

proc ::json::TokenState char {
    switch -glob -- $char {
        {} { return TokenState }
        { } - \t - \n { return TokenState }

        \[:,\{\}\] - \\[ - \\] {
            yield $char
            return TokenState
        }

        \\- { return NegativeNumberState }
        \[0-9\] { return [NumberState $char] }

        \" { return StringState }

        default { error "json::tokens: unexpected $char" }
    }
}

proc ::json::StringState char {
    variable strAccum
    variable strEscape

    if {$strEscape} {
        set strEscape no
        switch -- $char {
            \" { append strAccum \" }
            \/ { append strAccum / }
            \\ { append strAccum \\ }
            b { append strAccum \b }
            f { append strAccum \f }
            n { append strAccum \n }
            r { append strAccum \r }
            t { append strAccum \t }
            default { error "json::tokens: invalid string escape $char" }
        }
    } else {
        switch -- $char {
            {} { error "json::tokens: unexpected end of string" }
            \" {
                yield $strAccum
                set strAccum {}
                return TokenState
            }
            \\ { set strEscape yes }
            default { append strAccum $char }
        }
    }

    return StringState
}

proc ::json::NegativeNumberState char {
    variable numSign

    switch -glob -- $char {
        \[0-9\] {
            set numSign -1
            return [NumberState $char]
        }
        default { error "json::tokens: unexpected number $char" }
    }
}

proc ::json::NumberState char {
    variable numAccum
    variable numSign

    switch -glob -- $char {
        {[0-9]} {
            set numAccum [expr {$numAccum * 10 + $char}]
            return NumberState
        }
        default {
            yield [expr {$numSign * $numAccum}]
            set numAccum 0
            set numSign 1
            return [TokenState $char]
        }
    }
}

proc ::json::tokens channelID {
    yield

    set state TokenState

    while {[set char [read $channelID 1]] ne {}} {
        set state [$state $char]
    }

    # Check state allows EOF
    $state $char

    return $char
}

coroutine next json::tokens stdin

proc 1 {} {
    set sum 0

    while {[set token [next]] ne {}} {
        catch { incr sum $token }
    }

    puts $sum
}

proc 2 {} {
    set current [dict create red no sum 0]
    set member no
    set objects {}

    while {[set token [next]] ne {}} {
        switch -- $token {
            \{ {
                lappend objects $current
                set current [dict create red no sum 0]
                set member no
            }
            \} {
                set child $current
                set current [lindex $objects end]
                if {![dict get $child red]} {
                    dict incr current sum [dict get $child sum]
                }
                set objects [lreplace $objects end end]
                set member no
            }
            : {
                set member yes
            }
            red {
                if {$member} {
                    dict set current red yes
                }
                set member no
            }
            default {
                catch { dict incr current sum $token }
                set member no
            }
        }
    }

    puts [dict get $current sum]
}

$puzzle(part)
