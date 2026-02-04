package day21

import (
	"bufio"
	"fmt"
	"strconv"
)

type Game struct {
	position [2]int
	score    [2]int
	player   uint
}

func (s Game) Next(roll int) Game {
	var sNext = s
	sNext.position[s.player] = (sNext.position[s.player] + roll) % 10
	sNext.score[s.player] += sNext.position[s.player] + 1
	sNext.player = (s.player + 1) % 2
	return sNext
}

func ReadPosition(r *bufio.Reader) ([2]int, error) {
	var players [2]int

	for _ = range players {
		var err error
		var i, p int

		if _, err = fmt.Fscanf(r, "Player %d starting position: %d\n", &i, &p); err != nil {
			return [2]int{}, err
		}

		if i < 1 || i > 2 || players[i-1] != 0 {
			return [2]int{}, strconv.ErrRange
		}

		players[i-1] = p - 1
	}

	return players, nil
}
