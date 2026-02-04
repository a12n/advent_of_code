package day09

import (
	"bufio"
	"fmt"
	"io"
	"log"

	"a12n/advent_of_code/2021/lib"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid lib.Grid[int]

	if grid, err = lib.ReadDigitGrid(r); err != nil {
		return err
	}

	var extent = grid.Extent()
	var pos lib.Point2
	var top3 [3]int

	for pos = range AllLowPos(grid) {
		var n = len(Basin(grid, extent, pos))
		if n > top3[2] {
			top3[0], top3[1], top3[2] = top3[1], top3[2], n
		} else if n > top3[1] {
			top3[0], top3[1] = top3[1], n
		} else if n > top3[0] {
			top3[0] = n
		}
		log.Println("start", pos, "n", n, "top3", top3)
	}

	fmt.Fprintln(w, top3[0]*top3[1]*top3[2])

	return nil
}

func Basin(grid lib.Grid[int], extent lib.Extent2, start lib.Point2) lib.Set[lib.Point2] {
	var queue []lib.Point2
	var visitted = make(lib.Set[lib.Point2])

	queue = append(queue, start)
	visitted.Add(start)

	for len(queue) > 0 {
		var dir lib.Dir
		var pos lib.Point2

		pos, queue = queue[0], queue[1:]

		for _, dir = range lib.AllDir {
			var adj = pos.Add(dir.ToVector())

			if !extent.Contains(adj) {
				continue
			}

			if grid.Get(adj) == 9 {
				continue
			}

			if grid.Get(adj) <= grid.Get(pos) {
				continue
			}

			queue = append(queue, adj)
			visitted.Add(adj)
		}
	}

	return visitted
}
