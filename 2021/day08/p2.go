package day08

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math/bits"
	"slices"
	"strconv"
	"strings"
)

type Seg uint8

const (
	A Seg = 0b0000001
	B Seg = 0b0000010
	C Seg = 0b0000100
	D Seg = 0b0001000
	E Seg = 0b0010000
	F Seg = 0b0100000
	G Seg = 0b1000000

	AllSeg = 0b1111111
)

func ParseSeg(str string) (Seg, error) {
	var i int
	var s Seg

	for i = range str {
		switch str[i] {
		case 'a', 'A':
			s |= A
		case 'b', 'B':
			s |= B
		case 'c', 'C':
			s |= C
		case 'd', 'D':
			s |= D
		case 'e', 'E':
			s |= E
		case 'f', 'F':
			s |= F
		case 'g', 'G':
			s |= G
		default:
			return 0, strconv.ErrSyntax
		}
	}

	return s, nil
}

func (s Seg) Not() Seg {
	return (^s) & AllSeg
}

func (s Seg) OnesCount() int {
	return bits.OnesCount8(uint8(s))
}

func (s Seg) Value() int {
	return map[Seg]int{
		A | B | C | E | F | G:     0,
		C | F:                     1,
		A | C | D | E | G:         2,
		A | C | D | F | G:         3,
		B | C | D | F:             4,
		A | B | D | F | G:         5,
		A | B | D | E | F | G:     6,
		A | C | F:                 7,
		A | B | C | D | E | F | G: 8,
		A | B | C | D | F | G:     9,
	}[s]
}

type Wiring map[Seg]Seg

func NewWiring(patterns [10]Seg) (Wiring, error) {
	var wiring = make(Wiring)

	var i int
	var s Seg

	var bits [10]Seg
	var bits069 = make([]Seg, 0, 3)
	var bits235 = make([]Seg, 0, 3)

	for _, s = range patterns {
		switch s.OnesCount() {
		case 2:
			log.Printf("%07b: ones %d, n %d", s, 2, 1)
			bits[1] = s
		case 3:
			log.Printf("%07b: ones %d, n %d", s, 3, 7)
			bits[7] = s
		case 4:
			log.Printf("%07b: ones %d, n %d", s, 4, 4)
			bits[4] = s
		case 5:
			log.Printf("%07b: ones %d, n %d | %d | %d", s, 5, 2, 3, 5)
			bits235 = append(bits235, s)
		case 6:
			log.Printf("%07b: ones %d, n %d | %d | %d", s, 6, 0, 6, 9)
			bits069 = append(bits069, s)
		case 7:
			log.Printf("%07b: ones %d, n %d", s, 7, 8)
			bits[8] = s
		}
	}

	if bits[1] == 0 || bits[4] == 0 || bits[7] == 0 || bits[8] == 0 ||
		len(bits069) != 3 || len(bits235) != 3 {
		return nil, fmt.Errorf("inconsistent patterns")
	}

	// Segment A from bits of "7" and "1".
	s = bits[7] ^ bits[1]
	wiring[s] = A

	// Find "9".
	for i, bits[9] = range bits069 {
		s = ^(bits[4] | bits[7]) & bits[9]
		if s.OnesCount() == 1 {
			// Segment G from bits of "4", "7" and "9".
			bits069 = slices.Delete(bits069, i, i+1)
			wiring[s] = G
			break
		}
	}

	// Find "6".
	for i, bits[6] = range bits069 {
		s = bits[6] & bits[1]
		if s.OnesCount() == 1 {
			// Segment F from bits of "1" and "6".
			bits069 = slices.Delete(bits069, i, i+1)
			wiring[s] = F
			break
		}
	}

	// Find "0".
	bits[0] = bits069[0]
	bits069 = nil

	// Find "2".
	for i, bits[2] = range bits235 {
		s = bits[2] & bits[9].Not()
		if s.OnesCount() == 1 {
			// Segment E from bits of "9" and "2".
			bits235 = slices.Delete(bits235, i, i+1)
			wiring[s] = E
			break
		}
	}

	// Find "5".
	for i, bits[5] = range bits235 {
		s = bits[5] & bits[1]
		if s.OnesCount() == 1 {
			// Segment F from bits of "1" and "5".
			bits235 = slices.Delete(bits235, i, i+1)
			wiring[s] = F
			break
		}
	}

	// Find "3".
	bits[3] = bits235[0]
	bits235 = nil

	// Segment C from bits of "2" and "1".
	s = bits[2] & bits[1]
	wiring[s] = C

	// Segment D from bits of "8" and "0".
	s = bits[8] & bits[0].Not()
	wiring[s] = D

	// Segment B from bits of "9" and "3".
	s = bits[9] & bits[3].Not()
	wiring[s] = B

	return wiring, nil
}

func (w Wiring) Rewire(s Seg) Seg {
	var b, r Seg
	for _, b = range []Seg{A, B, C, D, E, F, G} {
		if (s & b) != 0 {
			r |= w[s&b]
		}
	}
	return r
}

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var sum int

	for {
		var line string
		var output [4]Seg
		var patterns [10]Seg
		var wiring Wiring

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		if patterns, output, err = ParseEntry2(line); err != nil {
			return err
		}

		if wiring, err = NewWiring(patterns); err != nil {
			return err
		}

		sum +=
			wiring.Rewire(output[0]).Value()*1000 +
				wiring.Rewire(output[1]).Value()*100 +
				wiring.Rewire(output[2]).Value()*10 +
				wiring.Rewire(output[3]).Value()
	}

	fmt.Fprintln(w, sum)

	return nil
}

func ParseEntry2(str string) (patterns [10]Seg, output [4]Seg, err error) {
	var fields []string
	var i int

	str = strings.TrimSpace(str)

	if fields = strings.Split(str, " | "); len(fields) != 2 {
		err = fmt.Errorf("bad entry %v", fields)
		return
	}

	for i, str = range strings.Split(fields[0], " ") {
		if i >= len(patterns) {
			err = fmt.Errorf("bad patterns %q", fields[0])
		}
		if patterns[i], err = ParseSeg(str); err != nil {
			return
		}
	}

	for i, str = range strings.Split(fields[1], " ") {
		if i >= len(output) {
			err = fmt.Errorf("bad output %q", fields[1])
			return
		}
		if output[i], err = ParseSeg(str); err != nil {
			return
		}
	}

	return

}
