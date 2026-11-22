package day24

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

// Input from stdin ignored, input file preprocessed as inputs.go
// file.
func Part1(r *bufio.Reader, w io.Writer) error {
	type link struct{ w, z int }

	var carry = [14 + 1]map[int]link{14: map[int]link{0: {}}}
	var digit int
	var l link

	for digit = 13; digit >= 0; digit-- {
		carry[digit] = make(map[int]link)
		for l.w = 1; l.w <= 9; l.w++ {
			for l.z, _ = range carry[digit+1] {
				var z = inputs[digit](l.w, l.z)
				carry[digit][z] = l
			}
		}
		log.Println(digit, "len(carry)", len(carry[digit]), carry[digit][0])
	}

	var model int64
	var mult int64 = 1

	l.z = 0
	for digit = 0; digit < 14; digit++ {
		l = carry[digit][l.z]
		log.Println(digit, "best", l)

		model += int64(l.w) * mult
		mult *= 10
	}

	fmt.Fprint(w, model)

	return nil
}
