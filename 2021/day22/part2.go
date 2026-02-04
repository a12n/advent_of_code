package day22

import (
	"bufio"
	"fmt"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var steps []Step

	if steps, err = ReadSteps(r); err != nil {
		return err
	}

	var reactor = RegionSet{}

	for i := range steps {
		if steps[i].On {
			reactor.Add(steps[i].Region)
		} else {
			reactor.Sub(steps[i].Region)
		}
	}

	fmt.Fprintln(w, reactor.Volume())

	return err
}
