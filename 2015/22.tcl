#!/usr/bin/env tclsh

set spells {
    "Magic Missile" { mana 53 boss { hp -4 } }
    "Drain" { mana 73 player { hp 2 } boss { hp -2 } }
    "Shield" { mana 113 turns 6 player { armor 7 } }
    "Poison" { mana 173 turns 6 cumulative yes boss { hp -3 } }
    "Recharge" { mana 229 turns 5 cumulative yes player { mana +101 } }
}

namespace eval ::character {
    proc isDead character {
        expr {[dict get $character hp] <= 0}
    }

    proc takeMeleeDamage {character damage} {
        if {[dict exists $character armor]} {
            set armor [dict get $character armor]
        } else {
            set armor 0
        }

        set hits [expr {-max(1, $damage - $armor)}]

        dict incr character hp $hits

        return $character
    }
}

namespace eval ::effect {
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
                dict incr entity $key [expr {$value * $mult}]
            }
        }
        return $entity
    }

    proc remove {effect entity} {
        add $effect $entity -1
    }
}

set globalManaSpentMin [expr {2**64 + 1}]

proc play {player boss {turn 0} {manaSpent 0} {effects {}}} {
    global globalManaSpentMin
    global spells

    if {$manaSpent > $globalManaSpentMin} {
        puts stderr "play $turn: manaSpent $manaSpent is greater than globalManaSpentMin $globalManaSpentMin"
        return {}
    }

    puts stderr "play $turn: player {$player} boss {$boss} manaSpent $manaSpent effects $effects"

    # Apply effects
    set effects [dict filter $effects script {name effect} {
        # Cumulative effects apply each turn while active.
        if {[effect::isCumulative $effect]} {
            puts stderr "play $turn: applying cumulative effect \"$name\""

            set boss [effect::add [effect::applicable $effect boss] $boss]
            set player [effect::add [effect::applicable $effect player] $player]

            if {[character::isDead $boss]} {
                puts stderr "play $turn: boss is dead"
                return $manaSpent
            }

            if {[character::isDead $player]} {
                puts stderr "play $turn: player is dead"
                return {}
            }
        }

        if {[dict incr effect turns -1] == 0} {
            # Undo non-cumulative effects as they deactivate.
            if {![effect::isCumulative $effect]} {
                puts stderr "play $turn: effect \"$name\" ended, unapplying"

                set boss [effect::remove [effect::applicable $effect boss] $boss]
                set player [effect::remove [effect::applicable $effect player] $player]

                if {[character::isDead $boss]} {
                    puts stderr "play $turn: boss is dead"
                    return $manaSpent
                }
                if {[character::isDead $player]} {
                    puts stderr "play $turn: player is dead"
                    return {}
                }
            }
            # Remove the effect
            expr no
        } else {
            # Keep the effect
            expr yes
        }
    }]

    if {$turn % 2 == 0} {
        puts stderr "play $turn: player turn"

        set manaSpentMin {}

        # Player turn, try each possible spell.
        dict for {name spell} $spells {
            if {$name in [dict keys $effects]} {
                # You cannot cast a spell that would start an effect
                # which is already active.
                puts stderr "play $turn: can't cast \"$name\", effect already active"
                continue
            }

            set spellMana [dict get $spell mana]

            if {$spellMana > [dict get $player mana]} {
                puts stderr "play $turn: can't cast \"$name\", takes $spellMana"
                continue
            }

            set playerNext $player
            set bossNext $boss
            set manaSpentNext [expr {$manaSpent + $spellMana}]
            set effectsNext $effects

            puts stderr "play $turn: player casts \"$name\""

            dict incr playerNext mana [expr {0 - $spellMana}]

            if {[effect::isProlonged $spell]} {
                if {![effect::isCumulative $spell]} {
                    set bossNext [effect::add [effect::applicable $spell boss] $bossNext]
                    set playerNext [effect::add [effect::applicable $spell player] $playerNext]
                }
                dict set effectsNext $name $spell
            } else {
                set bossNext [effect::add [effect::applicable $spell boss] $bossNext]
                set playerNext [effect::add [effect::applicable $spell player] $playerNext]
            }

            # TODO: Check at the top of the command.
            if {[character::isDead $bossNext]} {
                puts stderr "play $turn: boss is dead"
                return $manaSpentNext
            }
            if {[character::isDead $playerNext]} {
                puts stderr "play $turn: player is dead"
                return {}
            }

            set manaSpentChild [play $playerNext $bossNext [expr {$turn + 1}] $manaSpentNext $effectsNext]
            if {$manaSpentMin eq {} || $manaSpentChild < $manaSpentMin} {
                set manaSpentMin $manaSpentChild
            }
            if {$manaSpentMin ne {} && $manaSpentMin < $globalManaSpentMin} {
                set globalManaSpentMin $manaSpentMin
            }
        }

        puts stderr "play $turn: player waits"
        set manaSpentChild [play $player $boss [expr {$turn + 1}] $manaSpent $effects]
        if {$manaSpentMin eq {} || $manaSpentChild < $manaSpentMin} {
            set manaSpentMin $manaSpentChild
        }
        if {$manaSpentMin ne {} && $manaSpentMin < $globalManaSpentMin} {
            set globalManaSpentMin $manaSpentMin
        }

        # Minimum of all possible outcomes.
        puts stderr "play $turn: min mana spent on all possible actions: $manaSpentMin"
        return $manaSpentMin
    } else {
        puts stderr "play $turn: boss turn"

        set damage [dict get $boss damage]
        puts stderr "play $turn: boss attacks with $damage"
        set playerNext [character::takeMeleeDamage $player $damage]
        if {[character::isDead $playerNext]} {
            puts stderr "play $turn: player is dead"
            return {}
        }

        play $playerNext $boss [incr turn] $manaSpent $effects
    }
}

set player [dict create armor 0 hp 50 mana 500]
set boss [lsort -index 0 -stride 2 [string map {
    "Damage:" damage
    "Hit Points:" hp
} [read stdin]]]

puts stderr "player $player"
puts stderr "boss $boss"
play $player $boss
puts $globalManaSpentMin
