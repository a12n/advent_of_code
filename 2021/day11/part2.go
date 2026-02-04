package day11

import (
	"bufio"
	"fmt"
	"io"

	"a12n/advent_of_code/2021/lib"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid lib.GridSetter[int]

	if grid, err = lib.ReadDigitGrid(r); err != nil {
		return err
	}

	var extent = grid.Extent()
	var flashed lib.Set[lib.Point2]
	var steps int

	for flashed = range SimulateFlashes(grid) {
		steps++
		if len(flashed) == extent.Area() {
			break
		}
	}

	fmt.Fprintln(w, steps)

	return nil
}
