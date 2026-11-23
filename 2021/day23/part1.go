package day23

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
	"strconv"
	"strings"
)

type Amphipod byte

const (
	A Amphipod = iota
	B
	C
	D
)

func ParseAmphipod(str string) (Amphipod, error) {
	switch strings.ToUpper(strings.TrimSpace(str)) {
	case "A":
		return A, nil
	case "B":
		return B, nil
	case "C":
		return C, nil
	case "D":
		return D, nil
	default:
		return 0, strconv.ErrSyntax
	}
}

func (a Amphipod) Energy() int {
	return [4]int{1, 10, 100, 1000}[a]
}

func (a Amphipod) String() string {
	return [4]string{A: "A", B: "B", C: "C", D: "D"}[a]
}

type Spot byte

const (
	// Rooms
	A0 = Spot(A << 2)
	A1 = A0 + 1
	B0 = Spot(B << 2)
	B1 = B0 + 1
	C0 = Spot(C << 2)
	C1 = C0 + 1
	D0 = Spot(D << 2)
	D1 = D0 + 1

	BadRoom Spot = 0xff
	MinRoom      = A0
	MaxRoom      = D1

	// Hallway
	H0 = Spot(0b10000)
	H1 = H0 + 1
	H2 = H0 + 2
	H3 = H0 + 3
	H4 = H0 + 4
	H5 = H0 + 5
	H6 = H0 + 6

	MinHallway = H0
	MaxHallway = H6
)

func Home(amph Amphipod, index int) Spot {
	return Spot(amph<<2) + Spot(index)
}

func (s Spot) IsHallway() bool {
	return (s >> 4) == 1
}

func (s Spot) IsRoom() bool {
	return (s >> 4) == 0
}

func (s Spot) IsHome(a Amphipod) bool {
	return Amphipod(s>>2) == a
}

func (s Spot) HallwayIndex() int {
	return int(s) & 0b1111
}

func (s Spot) RoomIndex() int {
	return int(s) & 0b11
}

func (s Spot) String() string {
	return []string{
		A0: "A0",
		A1: "A1",
		B0: "B0",
		B1: "B1",
		C0: "C0",
		C1: "C1",
		D0: "D0",
		D1: "D1",
		H0: "H0",
		H1: "H1",
		H2: "H2",
		H3: "H3",
		H4: "H4",
		H5: "H5",
		H6: "H6",
	}[s]
}

// FIXME: Too complex
func (s Spot) Step(t Spot) (Spot, int) {
	if s == t {
		return s, 0
	}

	switch s {
	case A1, B1, C1, D1: // To get to anywhere from the bottom room, go to the top room
		return s - 1, 1

	case A0: // Top A
		switch t {
		case A1: // To the bottom room
			return A1, 1
		case H0, H1: // To the left
			return H1, 2
		case B0, B1, C0, C1, D0, D1, H2, H3, H4, H5, H6: // To the right
			return H2, 2
		}

	case B0:
		switch t {
		case B1: // Bottom room
			return B1, 1
		case A0, A1, H0, H1, H2: // To the left
			return H2, 2
		case C0, C1, D0, D1, H3, H4, H5, H6: // To the right
			return H3, 2
		}

	case C0:
		switch t {
		case C1:
			return C1, 1
		case A0, A1, B0, B1, H0, H1, H2, H3:
			return H3, 2
		case D0, D1, H4, H5, H6:
			return H4, 2
		}

	case D0:
		switch t {
		case D1:
			return D1, 1
		case A0, A1, B0, B1, C0, C1, H0, H1, H2, H3, H4:
			return H4, 2
		case H5, H6:
			return H5, 2
		}

	case H0:
		return H1, 1

	case H1:
		switch t {
		case H0:
			return H0, 1
		case A0, A1:
			return A0, 2
		case B0, B1, C0, C1, D0, D1, H2, H3, H4, H5, H6:
			return H2, 2
		}

	case H2:
		switch t {
		case A0, A1:
			return A0, 2
		case H0, H1:
			return H1, 2
		case B0, B1:
			return B0, 2
		case C0, C1, D0, D1, H3, H4, H5, H6:
			return H3, 2
		}

	case H3:
		switch t {
		case A0, A1, H0, H1, H2:
			return H2, 2
		case B0, B1:
			return B0, 2
		case C0, C1:
			return C0, 2
		case D0, D1, H4, H5, H6:
			return H4, 2
		}

	case H4:
		switch t {
		case A0, A1, B0, B1, H0, H1, H2, H3:
			return H3, 2
		case C0, C1:
			return C0, 2
		case D0, D1:
			return D0, 2
		case H5, H6:
			return H5, 2
		}

	case H5:
		switch t {
		case A0, A1, B0, B1, C0, C1, H0, H1, H2, H3, H4:
			return H4, 2
		case D0, D1:
			return D0, 2
		case H6:
			return H6, 1
		}

	case H6:
		return H5, 1
	}

	panic(fmt.Sprint(s, t))
}

func (s Spot) Path(t Spot) (path []Spot, dist int) {
	var d int
	for s != t {
		s, d = s.Step(t)
		path = append(path, s)
		dist = dist + d
	}
	return
}

type AmphipodSpotMap [4][2]Spot

func (amphs AmphipodSpotMap) String() string {
	return amphs[A][0].String() + amphs[A][1].String() + "-" +
		amphs[B][0].String() + amphs[B][1].String() + "-" +
		amphs[C][0].String() + amphs[C][1].String() + "-" +
		amphs[D][0].String() + amphs[D][1].String()
}

func (amphs AmphipodSpotMap) IsOccupied(s Spot) bool {
	return amphs[A][0] == s || amphs[A][1] == s ||
		amphs[B][0] == s || amphs[B][1] == s ||
		amphs[C][0] == s || amphs[C][1] == s ||
		amphs[D][0] == s || amphs[D][1] == s
}

func (amphs AmphipodSpotMap) IsOrganized() bool {
	return amphs[A][0].IsHome(A) && amphs[A][1].IsHome(A) &&
		amphs[B][0].IsHome(B) && amphs[B][1].IsHome(B) &&
		amphs[C][0].IsHome(C) && amphs[C][1].IsHome(C) &&
		amphs[D][0].IsHome(D) && amphs[D][1].IsHome(D)
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

	var amphs AmphipodSpotMap
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
