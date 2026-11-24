package day23

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var amphs FoldedAmphipodMap
	var err error

	if amphs, err = ReadFoldedAmphipodMap(r); err != nil {
		return err
	}

	log.Println("spots", amphs)

	fmt.Fprintln(w, MinEnergy(nil, &amphs))

	return nil
}

type FoldedAmphipodMap [4][2]Spot

func ReadFoldedAmphipodMap(r *bufio.Reader) (FoldedAmphipodMap, error) {
	var err error
	var rooms = [4][2]byte{}

	if _, err = fmt.Fscanf(r,
		"#############\n"+
			"#...........#\n"+
			"###%c#%c#%c#%c###\n"+
			"  #%c#%c#%c#%c#\n"+
			"  #########\n",
		&rooms[A][0], &rooms[B][0], &rooms[C][0], &rooms[D][0],
		&rooms[A][1], &rooms[B][1], &rooms[C][1], &rooms[D][1],
	); err != nil {
		return FoldedAmphipodMap{}, err
	}

	var amphs FoldedAmphipodMap
	var index [4]int
	var i, j int

	for i = range rooms {
		for j = range rooms[i] {
			var amph Amphipod

			if amph, err = ParseAmphipod(string(rooms[i][j])); err != nil {
				return FoldedAmphipodMap{}, err
			}

			if index[amph] > 1 {
				return FoldedAmphipodMap{}, fmt.Errorf("too many %v amphipods", amph)
			}

			amphs[amph][index[amph]] = HomeSpot(Amphipod(i), j)
			index[amph]++
		}
	}

	return amphs, nil
}

func (m FoldedAmphipodMap) String() string {
	return m[A][0].String() + m[A][1].String() + "-" +
		m[B][0].String() + m[B][1].String() + "-" +
		m[C][0].String() + m[C][1].String() + "-" +
		m[D][0].String() + m[D][1].String()
}

func (FoldedAmphipodMap) Num() int {
	return 2
}

func (m FoldedAmphipodMap) Occupied(s Spot) bool {
	return m[A][0] == s || m[A][1] == s ||
		m[B][0] == s || m[B][1] == s ||
		m[C][0] == s || m[C][1] == s ||
		m[D][0] == s || m[D][1] == s
}

func (m FoldedAmphipodMap) Organized() bool {
	return m[A][0].Home(A) && m[A][1].Home(A) &&
		m[B][0].Home(B) && m[B][1].Home(B) &&
		m[C][0].Home(C) && m[C][1].Home(C) &&
		m[D][0].Home(D) && m[D][1].Home(D)
}

func (m FoldedAmphipodMap) Get(amph Amphipod, index int) Spot {
	return m[amph][index]
}

func (m *FoldedAmphipodMap) Set(amph Amphipod, index int, s Spot) {
	(*m)[amph][index] = s
}

func (m FoldedAmphipodMap) Next(amph Amphipod, index int, s Spot) (spots []Spot) {
	var allowHome = func() {
		spots = append(spots, HomeSpot(amph, 1))       // allow bottom home
		if HomeSpot(amph, 1) == m[amph][(index+1)%2] { // other amph in bottom home?
			spots = append(spots, HomeSpot(amph, 0)) // allow top home
		}
	}

	var allowHallway = func() {
		spots = append(spots, H0, H1, H2, H3, H4, H5, H6) // allow hallway
	}

	if s.Room() { // in a room
		if s.Home(amph) { // in home room
			if HomeSpot(amph, 1) == s || // already in bottom home
				(HomeSpot(amph, 0) == s && // already in top home and other amph in bottom home
					HomeSpot(amph, 1) == m[amph][(index+1)%2]) {
				// do not move
				return
			}
		}
		allowHallway()
	} else { // in hallway
		allowHome()
	}

	return
}
