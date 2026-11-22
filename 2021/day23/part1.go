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

type Amphipod int

const (
	A Amphipod = iota
	B
	C
	D
)

func (a Amphipod) Energy() int {
	return [4]int{1, 10, 100, 1000}[a]
}

type Spot int

const (
	RoomA   = Spot(A << 1)
	RoomB   = Spot(B << 1)
	RoomC   = Spot(C << 1)
	RoomD   = Spot(D << 1)
	MinRoom = RoomA + 0
	MaxRoom = RoomD + 1

	Hallway    Spot = 0b1000
	MinHallway      = Hallway + 0
	MaxHallway      = Hallway + 6

	MinSpot = MinRoom
	MaxSpot = MaxHallway

	// Short spot identifiers
	A0 = RoomA + 0
	A1 = RoomA + 1
	B0 = RoomB + 0
	B1 = RoomB + 1
	C0 = RoomC + 0
	C1 = RoomC + 1
	D0 = RoomD + 0
	D1 = RoomD + 1
	H0 = Hallway + 0
	H1 = Hallway + 1
	H2 = Hallway + 2
	H3 = Hallway + 3
	H4 = Hallway + 4
	H5 = Hallway + 5
	H6 = Hallway + 6
)

func Home(amph Amphipod, index int) Spot {
	return Spot(amph<<1) + Spot(index)
}

func (s Spot) IsHallway() bool {
	return (s >> 3) == 1
}

func (s Spot) IsRoom() bool {
	return (s >> 3) == 0
}

func (s Spot) IsHome(a Amphipod) bool {
	return Amphipod(s>>1) == a
}

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

func (a Amphipod) String() string {
	return [4]string{A: "A", B: "B", C: "C", D: "D"}[a]
}

func (s Spot) String() string {
	return []string{
		RoomA + 0:   "A0",
		RoomA + 1:   "A1",
		RoomB + 0:   "B0",
		RoomB + 1:   "B1",
		RoomC + 0:   "C0",
		RoomC + 1:   "C1",
		RoomD + 0:   "D0",
		RoomD + 1:   "D1",
		Hallway + 0: "H0",
		Hallway + 1: "H1",
		Hallway + 2: "H2",
		Hallway + 3: "H3",
		Hallway + 4: "H4",
		Hallway + 5: "H5",
		Hallway + 6: "H6",
	}[s]
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
	// log.Println("Distance: amph", amph, "from", from, "to", to)

	// From hallway to room is the same as from room to hallway, but
	// only for the corresponding amphipods.
	if to.IsRoom() {
		if !to.IsHome(amph) {
			// log.Println("Distance: to wrong room")
			return false, 0
		}
		from, to = to, from
	}

	// From rooms to hallway.
	if !(from.IsRoom() && to.IsHallway()) {
		// log.Println("Distance: not from room to hallway")
		return false, 0
	}

	var dist int
	var left bool

	// Move out from a room.
	for from.IsRoom() {
		if (from & 1) == 1 {
			// Move from the second room to the first room.
			from--
			dist++
		} else {
			// Move from the first room to the hallway.
			switch Amphipod(from >> 1) {
			case A:
				// To a closest hallway spot either to the left or to the right.
				if to < Hallway+2 {
					from = Hallway + 1
					left = true
				} else {
					from = Hallway + 2
				}
			case B:
				if to < Hallway+3 {
					from = Hallway + 2
					left = true
				} else {
					from = Hallway + 3
				}
			case C:
				if to < Hallway+4 {
					from = Hallway + 3
					left = true
				} else {
					from = Hallway + 4
				}
			case D:
				if to < Hallway+5 {
					from = Hallway + 4
					left = true
				} else {
					from = Hallway + 5
				}
			}
			dist += 2
		}
		// Check the next position is unoccupied.
		if amphs.IsOccupied(from) {
			// log.Println("Distance: room spot", from, "occupied")
			return false, 0
		}
	}

	// Move to the closest hallway spot.
	if left {
		from, to = to, from
	}
	// log.Println("Distance: in hallway, from", from, "to", to, "dist", dist)
	for ; from != to; from++ {
		dist += [MaxHallway - MinHallway]int{1, 2, 2, 2, 2, 1}[from-MinHallway]
		// Check the next position is unoccupied.
		if amphs.IsOccupied(from) {
			// log.Println("Distance: hallway spot", from, "occupied")
			return false, 0
		}
	}

	// log.Println("Distance: dist", dist)

	return true, dist
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
	var s0 Spot

	energy = math.MaxInt
	for amph = A; amph <= D; amph++ {
		for index, s0 = range amphs[amph] {
			var s1 Spot
			var spots []Spot

			if s0.IsRoom() {
				if s0.IsHome(amph) {
					switch s0 & 1 {
					case 0:
						if Home(amph, 1) == amphs[amph][(index+1)%2] {
							// In the first home room. In the second
							// room is also amphipod of the same type
							// at home. Both no need to go anywhere.
							continue
						}
					case 1:
						// In the second home room, nowhere to go.
						continue
					}
				}

				// Can move only ot a hallway spot.
				for s1 = MinHallway; s1 <= MaxHallway; s1++ {
					spots = append(spots, s1)
				}
			} else {
				// Can move only to the second home room.
				spots = append(spots, Home(amph, 1))
				// Or to the first home room, if the second one is
				// occupied by an amphipod of the same type.
				if Home(amph, 1) == amphs[amph][(index+1)%2] {
					spots = append(spots, Home(amph, 0))
				}
			}

			// log.Println("amph", amph, "index", index, "s0", s0, "spots", spots)

			for _, s1 = range spots {
				var dist, energyNext int

				if ok, dist = amphs.Distance(amph, s0, s1); ok {
					// log.Println("s1", s1, "dist", dist)
					amphs[amph][index] = s1
					if energyNext = c.MinEnergy(amphs); energyNext != math.MaxInt {
						energy = min(energy, energyNext+dist*amph.Energy())
					}
					amphs[amph][index] = s0
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
		&rooms[RoomA+0], &rooms[RoomB+0], &rooms[RoomC+0], &rooms[RoomD+0],
		&rooms[RoomA+1], &rooms[RoomB+1], &rooms[RoomC+1], &rooms[RoomD+1],
	); err != nil {
		return AmphipodSpotMap{}, err
	}

	var amphs AmphipodSpotMap
	var index [4]int
	var room int

	for room = range rooms {
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
