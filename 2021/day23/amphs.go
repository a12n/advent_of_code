package day23

import (
	"fmt"
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
	return [4]string{"A", "B", "C", "D"}[a]
}

type Spot byte

const (
	// Rooms
	A0 = Spot(A << 2)
	A1 = A0 + 1
	A2 = A0 + 2
	A3 = A0 + 3
	B0 = Spot(B << 2)
	B1 = B0 + 1
	B2 = B0 + 2
	B3 = B0 + 3
	C0 = Spot(C << 2)
	C1 = C0 + 1
	C2 = C0 + 2
	C3 = C0 + 3
	D0 = Spot(D << 2)
	D1 = D0 + 1
	D2 = D0 + 2
	D3 = D0 + 3

	BadRoom Spot = 0xff
	MinRoom      = A0
	MaxRoom      = D3

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

func HomeSpot(amph Amphipod, index int) Spot {
	return Spot(amph<<2) + Spot(index)
}

// Room is false for hallway spots.
func (s Spot) Room() bool {
	return (s >> 4) == 0
}

func (s Spot) Home(amph Amphipod) bool {
	return s.Room() && Amphipod(s>>2) == amph
}

func (s Spot) RoomIndex() int {
	return int(s) & 0b11
}

func (s Spot) String() string {
	if s == BadRoom {
		return ""
	} else if s.Room() {
		return string([]byte{"ABCD"[s>>2], byte(s&0b11) + '0'})
	} else { // hallway
		return string([]byte{'H', byte(s&0b1111) + '0'})
	}
}

// FIXME: Too complex
func (s Spot) Step(t Spot) (Spot, int) {
	if s == t {
		return s, 0
	}

	switch s {
	case A3, B3, C3, D3: // To get to anywhere from the bottom room, go up
		return s - 1, 1

		// Up or down from the rooms 2
	case A2:
		switch t {
		case A3:
			return A3, 1
		default:
			return A1, 1
		}
	case B2:
		switch t {
		case B3:
			return B3, 1
		default:
			return B1, 1
		}
	case C2:
		switch t {
		case C3:
			return C3, 1
		default:
			return C1, 1
		}
	case D2:
		switch t {
		case D3:
			return D3, 1
		default:
			return D1, 1
		}

		// Up or down from the rooms 1
	case A1:
		switch t {
		case A2, A3:
			return A2, 1
		default:
			return A0, 1
		}
	case B1:
		switch t {
		case B2, B3:
			return B2, 1
		default:
			return B0, 1
		}
	case C1:
		switch t {
		case C2, C3:
			return C2, 1
		default:
			return C0, 1
		}
	case D1:
		switch t {
		case D2, D3:
			return D2, 1
		default:
			return D0, 1
		}

		// Top rooms
	case A0:
		switch t {
		case A1, A2, A3: // To the bottom room
			return A1, 1
		case H0, H1: // To the left
			return H1, 2
		default: // To the right
			return H2, 2
		}
	case B0:
		switch t {
		case B1, B2, B3: // Bottom rooms
			return B1, 1
		case A0, A1, A2, A3, H0, H1, H2: // To the left
			return H2, 2
		default: // To the right
			return H3, 2
		}
	case C0:
		switch t {
		case C1, C2, C3: // Bottom rooms
			return C1, 1
		case D0, D1, D2, D3, H4, H5, H6: // To the right
			return H4, 2
		default: // To the left
			return H3, 2
		}
	case D0:
		switch t {
		case D1, D2, D3: // Bottom rooms
			return D1, 1
		case H5, H6: // To the right
			return H5, 2
		default: // To the left
			return H4, 2
		}

		// Ends of hallway
	case H0:
		return H1, 1

	case H1:
		switch t {
		case H0: // left
			return H0, 1
		case A0, A1, A2, A3: // right down
			return A0, 2
		default: // right
			return H2, 2
		}

	case H2:
		switch t {
		case H0, H1: // left
			return H1, 2
		case A0, A1, A2, A3: // left down
			return A0, 2
		case B0, B1, B2, B3: // right down
			return B0, 2
		default: // right
			return H3, 2
		}

	case H3:
		switch t {
		case A0, A1, A2, A3, H0, H1, H2: // left
			return H2, 2
		case B0, B1, B2, B3: // left down
			return B0, 2
		case C0, C1, C2, C3: // right down
			return C0, 2
		case D0, D1, D2, D3, H4, H5, H6: // right
			return H4, 2
		}

	case H4:
		switch t {
		case H5, H6: // right
			return H5, 2
		case D0, D1, D2, D3: // right down
			return D0, 2
		case C0, C1, C2, C3: // left down
			return C0, 2
		default: // left
			return H3, 2
		}

	case H5:
		switch t {
		case H6: // right
			return H6, 1
		case D0, D1, D2, D3: // left down
			return D0, 2
		default: // left
			return H4, 2
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

type AmphipodMap interface {
	fmt.Stringer

	Num() int

	Occupied(Spot) bool
	Organized() bool

	Get(Amphipod, int) Spot
	Set(Amphipod, int, Spot)
	Next(Amphipod, int, Spot) []Spot
}

func HasPath(amphs AmphipodMap, amph Amphipod, from, to Spot) (bool, int) {
	var dist int
	var path []Spot
	var s Spot

	path, dist = from.Path(to)
	for _, s = range path {
		if amphs.Occupied(s) {
			return false, 0
		}
	}

	return true, dist
}

func MinEnergy(cache map[string]int, amphs AmphipodMap) int {
	if cache == nil {
		cache = make(map[string]int)
	}

	var energy int
	var key = amphs.String()
	var ok bool

	if energy, ok = cache[key]; ok {
		return energy
	}

	if amphs.Organized() {
		cache[key] = 0
		return 0
	}

	var amph Amphipod
	var from, to Spot
	var index int

	energy = math.MaxInt
	for amph = A; amph <= D; amph++ {
		for index = 0; index < amphs.Num(); index++ {
			from = amphs.Get(amph, index)

			for _, to = range amphs.Next(amph, index, from) {
				var dist, energy2 int

				if ok, dist = HasPath(amphs, amph, from, to); ok {
					amphs.Set(amph, index, to)
					if energy2 = MinEnergy(cache, amphs); energy2 != math.MaxInt {
						energy = min(energy, energy2+dist*amph.Energy())
					}
					amphs.Set(amph, index, from)
				}
			}
		}
	}

	cache[key] = energy

	return energy
}
