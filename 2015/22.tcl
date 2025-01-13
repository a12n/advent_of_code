#!/usr/bin/env tclsh

set spells {
    "Magic Missile" { mana 53 boss { hp -4 } }
    "Drain" { mana 73 player { hp 2 } boss { hp -2 } }
    "Shield" { mana 113 turns 6 player { armor 7 } }
    "Poison" { mana 173 turns 6 cumulative yes boss { hp -3 } }
    "Recharge" { mana 229 turns 5 cumulative yes player { mana +101 } }
}

namespace eval game {
    namespace eval character {
        proc isDead character {
            expr {[dict get $character hp] <= 0}
        }

        proc magicDamage {character damage} {
            set hits [expr {-$damage}]

            dict incr character hp $hits

            return $character
        }

        proc meleeDamage {character damage} {
            if {[dict exists $character armor]} {
                set armor [dict get $character armor]
            } else {
                set armor 0
            }

            set hits [expr {-max(1, $damage - $armor)}]
            puts stderr "meleeDamage: hits $hits"

            dict incr character hp $hits

            return $character
        }
    }

    namespace eval effect {
        proc isCumulative effect {
            if {[dict exists $effect cumulative]} {
                dict get $effect cumulative
            } else {
                return no
            }
        }

        proc isProlonged effect {
            if {[dict exists $effect turns]} {
                expr {[dict get $effect turns] > 0}
            } else {
                return no
            }
        }

        proc applicable {effect entityName} {
            if {[dict exists $effect $entityName]} {
                dict get $effect $entityName
            } else {
                return {}
            }
        }

        proc add {effect entity {mult 1}} {
            foreach {key value} $effect {
                if {$key in {armor hp mana}} {
                    set increment [expr {$value * $mult}]
                    dict incr entity $key $increment
                    puts stderr "effect::add: $key increment $increment"
                }
            }
            return $entity
        }

        proc remove {effect entity} {
            add $effect $entity -1
        }
    }

    namespace eval state {
        # Game state is a list {turn spent player boss effects}.
        # Compare by mana spent and then (if mana spent is the same)
        # by boss HP.
        proc compare {s t} {
            if {[set a [lindex $s 1]] != [set b [lindex $t 1]]} {
                expr {$a - $b}
            } else {
                expr {[dict get [lindex $s 3] hp] - [dict get [lindex $t 3] hp]}
            }
        }
    }
}

proc play {player boss} {
    global spells

    # Initial state: {turn spent player boss effects}
    set queue [list [list 0 0 $player $boss {}]]

    set minSpent [expr {2**64 + 1}]
    set seen [dict create]

    while {$queue ne {}} {
        puts stderr "play: queue [llength $queue] seen [dict size $seen]"

        # Dequeue the minimum state: breadth first.
        # set queue [lassign $queue state]

        # Dequeue the next state: depth first.
        set state [lindex $queue end]
        set queue [lreplace $queue end end]

        lassign $state turn spent player boss effects
        puts stderr "play $turn: spent $spent player $player boss $boss effects $effects"

        if {$spent > $minSpent} {
            puts stderr "play $turn: spent $spent more than min $minSpent"
            continue
        }

        set stateKey \
            [list \
                 [expr {$turn % 2}] \
                 [lsort -stride 2 -index 0 $player] \
                 [lsort -stride 2 -index 0 $boss] \
                 [lsort -stride 2 -index 0 \
                      [dict map {name effect} $effects \
                           { dict get $effect turns }]]]
        # puts stderr "stateKey $stateKey"
        if {[dict exists $seen $stateKey]} {
            puts stderr "play $turn: stateKey $stateKey already seen"
            continue
        } else {
            dict set seen $stateKey yes
        }

        # A final state.
        if {[game::character::isDead $boss]} {
            puts stderr "play $turn: boss is dead, spent $spent"
            set minSpent [expr {min($minSpent, $spent)}]
            continue
        } elseif {[game::character::isDead $player]} {
            puts stderr "play $turn: player is dead"
            continue
        }

        # Apply effects.
        dict for {name effect} $effects {
            # Cumulative effects apply each turn while active.
            if {[game::effect::isCumulative $effect]} {
                puts stderr "play $turn: applying cumulative \"$name\""
                set boss [game::effect::add [game::effect::applicable $effect boss] $boss]
                set player [game::effect::add [game::effect::applicable $effect player] $player]
                if {[game::character::isDead $boss] || [game::character::isDead $player]} {
                    # There's no point in updating the game state.
                    break
                }
            }
        }
        # End effects.
        foreach name [dict keys $effects] {
            set n [dict get $effects $name turns]
            dict set effects $name turns [expr {$n - 1}]
        }
        # Remove effects.
        set effects [dict filter $effects script {name effect} {
            # Undo non-cumulative effects as they deactivate.
            if {[set n [dict get $effect turns]] == 0} {
                puts stderr "play $turn: effects \"$name\" wears off"
                if {![game::effect::isCumulative $effect]} {
                    set boss [game::effect::remove [game::effect::applicable $effect boss] $boss]
                    set player [game::effect::remove [game::effect::applicable $effect player] $player]
                    if {[game::character::isDead $boss] || [game::character::isDead $player]} {
                        break
                    }
                }

                # Remove the effect
                expr no
            } else {
                # Keep the effect
                puts stderr "play $turn: effect \"$name\" timer is now $n"
                expr yes
            }
        }]

        # Either of characters may be dead after applying effects.
        if {[game::character::isDead $boss]} {
            puts stderr "play $turn: boss killed by effects, spent $spent"
            set minSpent [expr {min($minSpent, $spent)}]
            continue
        } elseif {[game::character::isDead $player]} {
            puts stderr "play $turn: player killed by effects"
            continue
        }

        set turn2 [expr {$turn + 1}]

        if {$turn % 2 == 0} {
            # Player turn. Cast a spell…
            dict for {name spell} $spells {
                if {$name in [dict keys $effects]} {
                    # You cannot cast a spell that would start an effect
                    # which is already active. Try next spell.
                    puts stderr "play $turn: can't cast \"$name\", effect already active"
                    continue
                }

                set mana [dict get $spell mana]
                if {$mana > [dict get $player mana]} {
                    # Not enough mana for the spell. Try next spell.
                    puts stderr "play $turn: can't cast \"$name\", requires $mana mana"
                    continue
                }

                set effects2 $effects
                set player2 $player
                set boss2 $boss

                # Spend mana.
                set spent2 [expr {$spent + $mana}]
                dict incr player2 mana [expr {-$mana}]
                puts stderr "play $turn: player casts \"$name\" for $mana"

                if {[game::effect::isProlonged $spell]} {
                    dict set effects2 $name $spell
                }

                if {![game::effect::isCumulative $spell]} {
                    # Cumulative spell effects must be applied at the
                    # begining of the turn. Non-cumulative spells must
                    # be applied right away.
                    set boss2 [game::effect::add [game::effect::applicable $spell boss] $boss2]
                    set player2 [game::effect::add [game::effect::applicable $spell player] $player2]
                }

                lappend queue [list $turn2 $spent2 $player2 $boss2 $effects2]
            }

            # …or just wait.
            if {[llength $effects] > 0} {
                lappend queue [list $turn2 $spent $player $boss $effects]
            }
        } else {
            # Boss turn
            set damage [dict get $boss damage]
            puts stderr "play $turn: boss attacks with damage $damage"
            set player2 [game::character::meleeDamage $player $damage]
            lappend queue [list $turn2 $spent $player2 $boss $effects]
        }

        # FIXME: Priority queue.
        set queue [lsort -command game::state::compare $queue]
    }

    return $minSpent
}

set player [dict create armor 0 hp 50 mana 500]
set boss [lsort -index 0 -stride 2 [string map {
    "Damage:" damage
    "Hit Points:" hp
} [read stdin]]]

catch { dict set player hp $env(PLAYER_HP) }
catch { dict set player mana $env(PLAYER_MANA) }

puts stderr "player $player"
puts stderr "boss $boss"
puts [play $player $boss]
