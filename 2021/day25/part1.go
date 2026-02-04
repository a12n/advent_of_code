package day25

import (
	"bufio"
	"fmt"
	"io"
	"strconv"

	"a12n/advent_of_code/2021/lib"
)

const (
	East  = 0b01
	South = 0b10

	New = 0b0100
	Old = 0b1000
)

const (
	X = iota
	Y
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid CucumberGrid

	if grid, err = ReadCucumberGrid(r); err != nil {
		return err
	}

	fmt.Fprintln(w, grid.NumSteps())
	// fmt.Fprintln(w, Simulate(grid, grid.Extent(), -1))

	return nil
}

type CucumberGrid struct {
	lib.ArrayGrid[int]
}

func ReadCucumberGrid(r *bufio.Reader) (grid CucumberGrid, err error) {
	grid.ArrayGrid, err = lib.ReadArrayGrid(r,
		func(b byte) (int, error) {
			switch b {
			case '.':
				return 0, nil
			case '>':
				return East, nil
			case 'v':
				return South, nil
			default:
				return 0, strconv.ErrSyntax
			}
		},
	)
	return
}

func (g CucumberGrid) NumSteps() int {
	var extent = g.Extent()
	var size = [2]int{extent.Size(X), extent.Size(Y)}

	var addX = func(p lib.Point2) lib.Point2 {
		return lib.Point2{(p[X] + 1) % size[X], p[Y]}
	}
	var addY = func(p lib.Point2) lib.Point2 {
		return lib.Point2{p[X], (p[Y] + 1) % size[Y]}
	}

	var moving = [2]bool{true, true}
	var steps int

	for ; moving[X] || moving[Y]; steps++ {
		moving[X] = g.Move(East, addX)
		moving[Y] = g.Move(South, addY)
	}

	return steps
}

func (g CucumberGrid) Move(elt int, add func(lib.Point2) lib.Point2) bool {
	var extent = g.Extent()
	var moved bool
	var p lib.Point2

	for p = range extent.AllPoints() {
		if g.Get(p) != elt {
			continue
		}

		var q = add(p)

		if g.Get(q) != 0 {
			continue
		}

		g.Set(q, New|elt)
		g.Set(p, Old|elt)
		moved = true
	}

	for p = range extent.AllPoints() {
		elt = g.Get(p)
		switch elt & 0b1100 {
		case Old:
			g.Set(p, 0)
		case New:
			g.Set(p, elt&0b11)
		}
	}

	return moved
}
