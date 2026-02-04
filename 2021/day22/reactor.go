package day22

import (
	"bufio"
	"fmt"
	"io"
	"strconv"

	"a12n/advent_of_code/2021/lib"
)

type Step struct {
	Region lib.Extent3
	On     bool
}

func ReadStep(r *bufio.Reader) (Step, error) {
	var err error
	var state string
	var step Step

	if _, err = fmt.Fscanf(r,
		"%s x=%d..%d,y=%d..%d,z=%d..%d\n",
		&state,
		&step.Region.Begin[X], &step.Region.End[X],
		&step.Region.Begin[Y], &step.Region.End[Y],
		&step.Region.Begin[Z], &step.Region.End[Z],
	); err != nil {
		return Step{}, err
	}

	step.Region.End = step.Region.End.Add(lib.Vector3{1, 1, 1})

	switch state {
	case "on":
		step.On = true
	case "off":
		step.On = false
	default:
		return Step{}, strconv.ErrSyntax
	}

	return step, nil
}

func ReadSteps(r *bufio.Reader) ([]Step, error) {
	var steps []Step

	for {
		var err error
		var step Step

		if step, err = ReadStep(r); err != nil {
			if err == io.EOF {
				return steps, nil
			}
			return nil, err
		}

		steps = append(steps, step)
	}
}
