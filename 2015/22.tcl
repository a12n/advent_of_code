#!/usr/bin/env tclsh

set spells {
    name "Magic Missile" mana 53 damage 4
    name "Drain" mana 73 damage 2 hp +2
    name "Shield" mana 113 effect { turns 6 armor 7 }
    name "Poison" mana 173 effect { turns 6 damage 3 cumulative yes }
    name "Recharge" mana 229 effect { turns 5 mana +101 cumulative yes }
}

set boss [lsort -index 0 -stride 2 [string map {
    "Damage:" damage
    "Hit Points:" hp
} [read stdin]]]
set player [lsort -index 0 -stride 2 {hp 50 armor 0 mana 500}]

puts stderr "boss $boss"
puts stderr "player $player"
