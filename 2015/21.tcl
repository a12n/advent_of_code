# Branch and bound?

# You must buy exactly one weapon.
# 5 combinations
set weapons {
    {name "Dagger"     cost  8 damage 4 armor 0}
    {name "Greataxe"   cost 74 damage 8 armor 0}
    {name "Longsword"  cost 40 damage 7 armor 0}
    {name "Shortsword" cost 10 damage 5 armor 0}
    {name "Warhammer"  cost 25 damage 6 armor 0}
}

# Armor is optional, but you can't use more than one.
# 1+5=6 combinations.
set armor {
    {name "Leather"    cost  13 damage 0 armor 1}
    {name "Chainmail"  cost  31 damage 0 armor 2}
    {name "Splintmail" cost  53 damage 0 armor 3}
    {name "Bandedmail" cost  75 damage 0 armor 4}
    {name "Platemail"  cost 102 damage 0 armor 5}
}

# You can buy 0-2 rings.
# 1+6+15=22 combinations.
set rings {
    {name "Damage +1"  cost  25 damage 1 armor 0}
    {name "Damage +2"  cost  50 damage 2 armor 0}
    {name "Damage +3"  cost 100 damage 3 armor 0}
    {name "Defense +1" cost  20 damage 0 armor 1}
    {name "Defense +2" cost  40 damage 0 armor 2}
    {name "Defense +3" cost  80 damage 0 armor 3}
}

# There's only 16 items. Each of them may be taken or not. It's only
# 2^16 combinations (most of which are invalid).
# Valid: 5*6*22=660

proc wear {player item} {
    foreach stat {armor damage} {
        dict incr player $stat [dict get $item $stat]
    }
    return $player
}

# decrease in HP due to an attack.
proc hits {attacker defender} {
    expr {max(1, [dict get $attacker damage] - [dict get $defender armor])}
}

# Returns true if player wins the fight.
proc fight {player boss} {
    set bossHP [dict get $boss hp]
    set bossHits [hits $boss $player]
    set playerHP [dict get $player hp]
    set playerHits [hits $player $boss]

    # Number of rounds player/boss will keep the damage, in 1/100000
    # of a round.
    set playerRounds [expr {100000 * $playerHP / $bossHits}]
    set bossRounds [expr {100000 * $bossHP / $playerHits}]

    expr {$playerRounds >= $bossRounds}
}

set boss [lsort -index 0 -stride 2 [string map {
    "Armor:" armor
    "Damage:" damage
    "Hit Points:" hp
} [read stdin]]]
set player [lsort -index 0 -stride 2 {hp 100 damage 0 armor 0}]

puts stderr "boss $boss"
puts stderr "player $player"
