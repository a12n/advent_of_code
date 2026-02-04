package day21

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var game Game

	if game.position, err = ReadPosition(r); err != nil {
		return err
	}

	log.Printf("game %#v", game)
	var wins = (make(Cache)).NumWins(game)
	log.Printf("wins %#v", wins)

	fmt.Fprintln(w, max(wins[0], wins[1]))

	return err
}

type Cache map[Game][2]int

func (c Cache) NumWins(s Game) [2]int {
	var found bool
	var wins [2]int

	if wins, found = c[s]; found {
		return wins
	}

	if s.score[0] > 20 {
		wins[0] = 1
	} else if s.score[1] > 20 {
		wins[1] = 1
	} else {
		var dice [3]int

		for _, dice[0] = range [3]int{1, 2, 3} {
			for _, dice[1] = range [3]int{1, 2, 3} {
				for _, dice[2] = range [3]int{1, 2, 3} {
					var nums = c.NumWins(s.Next(dice[0] + dice[1] + dice[2]))
					wins[0] += nums[0]
					wins[1] += nums[1]
				}
			}
		}
	}

	c[s] = wins

	return wins
}
