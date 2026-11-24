package day23

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var folded FoldedAmphipodMap

	if folded, err = ReadFoldedAmphipodMap(r); err != nil {
		return err
	}

	log.Println("folded", folded)
	var full = NewFullAmphipodMap(folded)
	log.Println("full", full)

	fmt.Fprintln(w, MinEnergy(nil, &full))

	return nil
}

type FullAmphipodMap [2]FoldedAmphipodMap

func NewFullAmphipodMap(folded FoldedAmphipodMap) (full FullAmphipodMap) {
	var amph Amphipod
	var index int

	// Move from row 1 to row 3
	full[0] = folded
	for amph = A; amph <= D; amph++ {
		for index = 0; index < 2; index++ {
			if full[0][amph][index].RoomIndex() == 1 {
				full[0][amph][index] += 2
			}
		}
	}

	// Add extra amphipods
	// full[1][A][0] = D1
	// full[1][A][1] = C2
	// full[1][B][0] = C1
	// full[1][B][1] = B2
	// full[1][C][0] = B1
	// full[1][C][1] = D2
	// full[1][D][0] = A1
	// full[1][D][1] = A2
	full[1] = FoldedAmphipodMap{
		{D1, C2},
		{C1, B2},
		{B1, D2},
		{A1, A2},
	}

	return
}

func (m FullAmphipodMap) String() string {
	return m[0][A][0].String() + m[0][A][1].String() + m[1][A][0].String() + m[1][A][1].String() + "-" +
		m[0][B][0].String() + m[0][B][1].String() + m[1][B][0].String() + m[1][B][1].String() + "-" +
		m[0][C][0].String() + m[0][C][1].String() + m[1][C][0].String() + m[1][C][1].String() + "-" +
		m[0][D][0].String() + m[0][D][1].String() + m[1][D][0].String() + m[1][D][1].String()
}

func (FullAmphipodMap) Num() int {
	return 4
}

func (m FullAmphipodMap) Occupied(s Spot) bool {
	return m[0].Occupied(s) || m[1].Occupied(s)
}

func (m FullAmphipodMap) Organized() bool {
	return m[0].Organized() && m[1].Organized()
}

func (m FullAmphipodMap) Get(amph Amphipod, index int) Spot {
	return m[index>>1][amph][index&1]
}

func (m *FullAmphipodMap) Set(amph Amphipod, index int, s Spot) {
	(*m)[index>>1][amph][index&1] = s
}

func (m FullAmphipodMap) Next(amph Amphipod, index int, s Spot) (spots []Spot) {
	var atHome [4]bool
	var jndex int

	for jndex = 0; jndex < 4; jndex++ {
		if jndex == index {
			continue
		}
		atHome[3] = (atHome[3] || HomeSpot(amph, 3) == m.Get(amph, jndex))
		atHome[2] = (atHome[2] || HomeSpot(amph, 2) == m.Get(amph, jndex))
		atHome[1] = (atHome[1] || HomeSpot(amph, 1) == m.Get(amph, jndex))
		atHome[0] = (atHome[0] || HomeSpot(amph, 0) == m.Get(amph, jndex))
	}

	var allowHome = func() {
		spots = append(spots, HomeSpot(amph, 3)) // allow bottom home
		if atHome[3] {
			spots = append(spots, HomeSpot(amph, 2))
			if atHome[2] {
				spots = append(spots, HomeSpot(amph, 1))
				if atHome[1] {
					spots = append(spots, HomeSpot(amph, 0))
				}
			}
		}
	}

	var allowHallway = func() {
		spots = append(spots, H0, H1, H2, H3, H4, H5, H6)
	}

	if s.Room() { // in a room
		if s.Home(amph) { // in home room
			// TODO
		} else { // in other room
			allowHome()
		}
		allowHallway()
	} else { // in hallway
		allowHome()
	}

	return
}
