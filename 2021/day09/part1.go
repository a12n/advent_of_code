package day09

import (
	"bufio"
	"fmt"
	"io"
	"iter"

	"a12n/advent_of_code/2021/lib"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid lib.Grid[int]

	if grid, err = lib.ReadDigitGrid(r); err != nil {
		return err
	}

	var pos lib.Point2
	var sum int

	for pos = range AllLowPos(grid) {
		sum += grid.Get(pos) + 1
	}

	fmt.Fprintln(w, sum)

	return nil
}

func AllLowPos(grid lib.Grid[int]) iter.Seq[lib.Point2] {
	return func(yield func(lib.Point2) bool) {
		var dir lib.Dir
		var extent = grid.Extent()
		var pos lib.Point2
	NextPos:
		for pos = range extent.AllPoints() {
			for _, dir = range lib.AllDir {
				var adj = pos.Add(dir.ToVector())
				if extent.Contains(adj) {
					if !(grid.Get(adj) > grid.Get(pos)) {
						continue NextPos
					}
				}
			}
			if !yield(pos) {
				return
			}
		}
	}
}
