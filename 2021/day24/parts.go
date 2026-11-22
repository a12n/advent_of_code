package day24

import (
	"bufio"
	"fmt"
	"io"
	"iter"
)

// Input from stdin ignored, input file preprocessed as inputs.go
// file.
func ModelNumber(digits iter.Seq[int]) int64 {
	type link struct{ w, z int }

	var carry = [14 + 1]map[int]link{14: map[int]link{0: {}}}
	var place int
	var l link

	for place = 13; place >= 0; place-- {
		carry[place] = make(map[int]link)
		for l.w = range digits {
			for l.z, _ = range carry[place+1] {
				var z = inputs[place](l.w, l.z)
				carry[place][z] = l
			}
		}
	}

	var model int64
	var mult int64 = 1

	l.z = 0
	for place = 0; place < 14; place++ {
		l = carry[place][l.z]
		model += int64(l.w) * mult
		mult *= 10
	}

	return model
}

func Part1(r *bufio.Reader, w io.Writer) error {
	fmt.Fprintln(w, ModelNumber(
		func(yield func(int) bool) {
			for digit := 1; digit <= 9; digit++ {
				_ = yield(digit)
			}
		},
	))
	return nil
}

func Part2(r *bufio.Reader, w io.Writer) error {
	fmt.Fprintln(w, ModelNumber(
		func(yield func(int) bool) {
			for digit := 9; digit >= 1; digit-- {
				_ = yield(digit)
			}
		},
	))
	return nil
}
