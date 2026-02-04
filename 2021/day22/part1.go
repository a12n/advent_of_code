package day22

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
	Z
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var steps []Step

	if steps, err = ReadSteps(r); err != nil {
		return err
	}

	var p lib.Point3
	var reactor = make(map[lib.Point3]struct{})
	var step Step
	var update func(lib.Point3)

	for _, step = range steps {
		if step.On {
			update = func(p lib.Point3) {
				reactor[p] = struct{}{}
			}
		} else {
			update = func(p lib.Point3) {
				delete(reactor, p)
			}
		}

		for p[X] = max(step.Region.Begin[X], -50); p[X] <= min(step.Region.End[X]-1, 50); p[X]++ {
			for p[Y] = max(step.Region.Begin[Y], -50); p[Y] <= min(step.Region.End[Y]-1, 50); p[Y]++ {
				for p[Z] = max(step.Region.Begin[Z], -50); p[Z] <= min(step.Region.End[Z]-1, 50); p[Z]++ {
					update(p)
				}
			}
		}
	}

	log.Println("reactor", reactor)

	fmt.Fprintln(w, len(reactor))

	return err
}
