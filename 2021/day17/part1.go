package day17

import (
	"bufio"
	"fmt"
	"io"
	"log"

	"a12n/advent_of_code/2021/lib"
)

const (
	X = iota
	Y
)

// x(t) = x0 + v(t) t
//
// x = x0 + v t
// v = v0 + a t
//
// x = x0 + (v0 + 1/2 a t) t
// x = x0 + v0 t + 1/2 a t^2
// x0 = 0
//
// x = v0 t + 1/2 a t^2
// a = { -1|0, -1 }

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var target lib.Extent2

	if _, err = fmt.Fscanf(r,
		"target area: x=%d..%d, y=%d..%d",
		&target.Begin[X], &target.End[X],
		&target.Begin[Y], &target.End[Y],
	); err != nil {
		return err
	}
	target.End = target.End.Add(lib.Vector2{1, 1})

	log.Printf("target %#v", target)

	return err
}
