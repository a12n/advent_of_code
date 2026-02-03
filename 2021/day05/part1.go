package day05

import (
	"bufio"
	"fmt"
	"io"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var grid = make(map[Point]int)
	var n int
	var p Point

	for {
		var err error
		var l Line

		if l, err = ReadLine(r); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		if !(l.IsHoriz() || l.IsVert()) {
			continue
		}

		for p = range l.AllPoints() {
			grid[p]++
		}
	}

	for p, n = range grid {
		if n < 2 {
			delete(grid, p)
		}
	}

	fmt.Fprintln(w, len(grid))

	return nil
}
