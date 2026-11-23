package day23

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
)

type AmphipodSpotMap [4][4]Spot

func NewAmphipodSpotMap() AmphipodSpotMap {
	return AmphipodSpotMap{
		{BadRoom, BadRoom, BadRoom, BadRoom},
		{BadRoom, BadRoom, BadRoom, BadRoom},
		{BadRoom, BadRoom, BadRoom, BadRoom},
		{BadRoom, BadRoom, BadRoom, BadRoom},
	}
}

func (amphs AmphipodSpotMap) IsFull() bool {
	return amphs[A][2] != BadRoom && amphs[A][3] != BadRoom &&
		amphs[B][2] != BadRoom && amphs[B][3] != BadRoom &&
		amphs[C][2] != BadRoom && amphs[C][3] != BadRoom &&
		amphs[D][2] != BadRoom && amphs[D][3] != BadRoom
}

func (amphs AmphipodSpotMap) String() string {
	return amphs[A][0].String() + amphs[A][1].String() +
		amphs[A][2].String() + amphs[A][3].String() + "-" +
		amphs[B][0].String() + amphs[B][1].String() +
		amphs[B][2].String() + amphs[B][3].String() + "-" +
		amphs[C][0].String() + amphs[C][1].String() +
		amphs[C][2].String() + amphs[C][3].String() + "-" +
		amphs[D][0].String() + amphs[D][1].String() +
		amphs[D][2].String() + amphs[D][3].String()
}

func (amphs AmphipodSpotMap) IsOccupied(s Spot) bool {
	return amphs[A][0] == s || amphs[A][1] == s ||
		amphs[A][2] == s || amphs[A][3] == s ||
		amphs[B][0] == s || amphs[B][1] == s ||
		amphs[B][2] == s || amphs[B][3] == s ||
		amphs[C][0] == s || amphs[C][1] == s ||
		amphs[C][2] == s || amphs[C][3] == s ||
		amphs[D][0] == s || amphs[D][1] == s ||
		amphs[D][2] == s || amphs[D][3] == s
}

func (amphs AmphipodSpotMap) IsOrganized() bool {
	return amphs[A][0].IsHome(A) && amphs[A][1].IsHome(A) &&
		amphs[B][0].IsHome(B) && amphs[B][1].IsHome(B) &&
		amphs[C][0].IsHome(C) && amphs[C][1].IsHome(C) &&
		amphs[D][0].IsHome(D) && amphs[D][1].IsHome(D) &&
		(!amphs.IsFull() ||
			(amphs[A][2].IsHome(A) && amphs[A][3].IsHome(A) &&
				amphs[B][2].IsHome(B) && amphs[B][3].IsHome(B) &&
				amphs[C][2].IsHome(C) && amphs[C][3].IsHome(C) &&
				amphs[D][2].IsHome(D) && amphs[D][3].IsHome(D)))
}

func (amphs AmphipodSpotMap) Distance(amph Amphipod, from, to Spot) (bool, int) {
	var dist int
	var path []Spot
	var s Spot

	path, dist = from.Path(to)
	for _, s = range path {
		if amphs.IsOccupied(s) {
			return false, 0
		}
	}

	return true, dist
}

// TODO: Full
func (amphs AmphipodSpotMap) Next(amph Amphipod, index int, from Spot) (spots []Spot) {
	if from.IsRoom() {
		if from.IsHome(amph) {
			switch from & 1 {
			case 0:
				if Home(amph, 1) == amphs[amph][(index+1)%2] {
					// In the first home room. In the second
					// room is also amphipod of the same type
					// at home. Both no need to go anywhere.
					return
				}
			case 1:
				// In the second home room, nowhere to go.
				return
			}
		}

		// Can move only ot a hallway spot.
		spots = append(spots, H0, H1, H2, H3, H4, H5, H6)
	} else {
		// Can move only to the second home room.
		spots = append(spots, Home(amph, 1))
		// Or to the first home room, if the second one is
		// occupied by an amphipod of the same type.
		if Home(amph, 1) == amphs[amph][(index+1)%2] {
			spots = append(spots, Home(amph, 0))
		}
	}

	return
}

func Part1(r *bufio.Reader, w io.Writer) error {
	var amphs AmphipodSpotMap
	var err error

	if amphs, err = ReadInput(r); err != nil {
		return err
	}

	log.Println("spots", amphs)

	fmt.Fprintln(w, NewCache().MinEnergy(amphs))

	return nil
}

type Cache map[AmphipodSpotMap]int

func NewCache() Cache {
	return make(map[AmphipodSpotMap]int)
}

func (c Cache) MinEnergy(amphs AmphipodSpotMap) int {
	var energy int
	var ok bool

	// log.Println("MinEnergy: amphs", amphs)

	if energy, ok = c[amphs]; ok {
		return energy
	}

	if amphs.IsOrganized() {
		log.Println("MinEnergy: organized", amphs)
		c[amphs] = 0
		return 0
	}

	var amph Amphipod
	var index int
	var from, to Spot

	energy = math.MaxInt
	for amph = A; amph <= D; amph++ {
		for index, from = range amphs[amph] {
			if from == BadRoom {
				continue
			}

			for _, to = range amphs.Next(amph, index, from) {
				var dist, energyNext int

				if ok, dist = amphs.Distance(amph, from, to); ok {
					// log.Println("s1", s1, "dist", dist)
					amphs[amph][index] = to
					if energyNext = c.MinEnergy(amphs); energyNext != math.MaxInt {
						energy = min(energy, energyNext+dist*amph.Energy())
					}
					amphs[amph][index] = from
				}
			}
		}
	}

	if energy != math.MaxInt {
		log.Println("MinEnergy: amphs", amphs, "energy", energy)
	}
	c[amphs] = energy

	return energy
}

func ReadInput(r *bufio.Reader) (AmphipodSpotMap, error) {
	var err error
	var rooms = [MaxRoom - MinRoom + 1]byte{}
	if _, err = fmt.Fscanf(r,
		"#############\n"+
			"#...........#\n"+
			"###%c#%c#%c#%c###\n"+
			"  #%c#%c#%c#%c#\n"+
			"  #########\n",
		&rooms[A0], &rooms[B0], &rooms[C0], &rooms[D0],
		&rooms[A1], &rooms[B1], &rooms[C1], &rooms[D1],
	); err != nil {
		return AmphipodSpotMap{}, err
	}

	var amphs = NewAmphipodSpotMap()
	var index [4]int
	var room int

	for room = range rooms {
		if rooms[room] == 0 {
			continue
		}

		var amph Amphipod

		if amph, err = ParseAmphipod(string(rooms[room])); err != nil {
			return AmphipodSpotMap{}, err
		}

		if index[amph] > 1 {
			return AmphipodSpotMap{}, fmt.Errorf("too many %v amphipods", amph)
		}

		amphs[amph][index[amph]] = Spot(room)
		index[amph]++
	}

	return amphs, nil
}
