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
	var digit int
	var l link

	for digit = 13; digit >= 0; digit-- {
		carry[digit] = make(map[int]link)
		for l.w = range digits {
			for l.z, _ = range carry[digit+1] {
				var z = inputs[digit](l.w, l.z)
				carry[digit][z] = l
			}
		}
	}

	var model int64
	var mult int64 = 1

	l.z = 0
	for digit = 0; digit < 14; digit++ {
		l = carry[digit][l.z]
		model += int64(l.w) * mult
		mult *= 10
	}

	return model
}

func Part1(r *bufio.Reader, w io.Writer) error {
	var largest = func(yield func(int) bool) {
		for digit := 1; digit <= 9; digit++ {
			if !yield(digit) {
				return
			}
		}
	}
	fmt.Fprintln(w, ModelNumber(largest))
	return nil
}
