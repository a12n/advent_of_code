package day14

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
	"slices"
	"strconv"
	"strings"
)

type Pair [2]byte

func (p Pair) String() string {
	return string(p[:])
}

type PairFreqs map[Pair]int

func NewPairFreqs(str string) PairFreqs {
	var freqs = make(PairFreqs)
	var i int

	for i = 1; i < len(str); i++ {
		freqs[Pair{str[i-1], str[i]}]++
	}

	return freqs
}

func (f PairFreqs) ElementFreqs(first byte) ElementFreqs {
	var ans = ElementFreqs{first: 1}
	var n int
	var pair Pair

	for pair, n = range f {
		ans[pair[1]] += n
	}

	log.Printf("ElementFreqs %v", ans)

	return ans
}

func (f PairFreqs) String() string {
	var keys []string
	var pair Pair

	for pair, _ = range f {
		keys = append(keys, string(pair[:]))
	}

	slices.Sort(keys)

	var key string
	var s strings.Builder
	var sep bool

	for _, key = range keys {
		if sep {
			s.WriteByte(',')
		}
		s.WriteString(key)
		s.WriteByte('=')
		s.WriteString(strconv.Itoa(f[Pair{key[0], key[1]}]))
		sep = true
	}

	return s.String()
}

type ElementFreqs map[byte]int

func (f ElementFreqs) Skew() int {
	var least, most = math.MaxInt, 0
	var n int

	for _, n = range f {
		least = min(least, n)
		most = max(most, n)
	}

	return most - least
}

type PairRules map[Pair]byte

func ReadPairRules(r *bufio.Reader) (PairRules, error) {
	var err error
	var insert = make(PairRules)

	for {
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		var from Pair
		var to byte

		if _, err = fmt.Sscanf(line, "%c%c -> %c", &from[0], &from[1], &to); err != nil {
			return nil, err
		}

		insert[from] = to
	}

	return insert, nil
}

func (rules PairRules) Apply(freqs PairFreqs, times int) PairFreqs {
	for ; times > 0; times-- {
		log.Printf("times %d, freqs %p %v", times, freqs, freqs)

		var n int
		var pair Pair
		var tmp = make(PairFreqs)

		for pair, n = range freqs {
			var insert = rules[pair]

			if insert != 0 {
				var left = Pair{pair[0], insert}
				var right = Pair{insert, pair[1]}

				log.Printf("%s -> insert %s %s", pair, left, right)
				tmp[left] += n
				tmp[right] += n
			} else {
				log.Printf("%s -> keep %s", pair, pair)
				tmp[pair] += n
			}
		}

		freqs = tmp
	}

	log.Printf("times %d, freqs %p %v", times, freqs, freqs)
	return freqs
}
