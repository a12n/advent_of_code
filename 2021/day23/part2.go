package day23

import (
	"bufio"
	"io"
	"log"
)

func Unfold(amphs AmphipodSpotMap) AmphipodSpotMap {
	var amph Amphipod
	var index int

	// Move from row 1 to row 3
	for amph = A; amph <= D; amph++ {
		for index = 0; index < 2; index++ {
			if amphs[amph][index].RoomIndex() > 0 {
				amphs[amph][index] += 2
			}
		}
	}

	// Add extra amphipods
	amphs[A][2] = D1
	amphs[A][3] = C2
	amphs[B][2] = C1
	amphs[B][3] = B2
	amphs[C][2] = B1
	amphs[C][3] = D2
	amphs[D][2] = A1
	amphs[D][3] = A2

	return amphs
}

func Part2(r *bufio.Reader, w io.Writer) error {
	var amphs AmphipodSpotMap
	var err error

	if amphs, err = ReadInput(r); err != nil {
		return err
	}

	log.Println("folded", amphs)
	amphs = Unfold(amphs)
	log.Println("full", amphs)

	return nil
}
