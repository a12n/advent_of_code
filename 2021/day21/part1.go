package day21

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var game Game

	if game.position, err = ReadPosition(r); err != nil {
		return err
	}

	log.Printf("game %#v", game)

	var dice, move int

	for ; game.score[0] < 1000 && game.score[1] < 1000; move++ {
		game = game.Next((dice + 1) + (dice + 2) + (dice + 3))
		dice = (dice + 3) % 100
	}

	log.Printf("move %d, game %#v", move, game)

	fmt.Fprintln(w, (3*move)*game.score[game.player])

	return err
}
