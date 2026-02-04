package day11

import (
	"bufio"
	"fmt"
	"io"
	"iter"

	"a12n/advent_of_code/2021/lib"
)

const (
	X = iota
	Y
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid lib.GridSetter[int]

	if grid, err = lib.ReadDigitGrid(r); err != nil {
		return err
	}

	var flashed lib.Set[lib.Point2]
	var num int
	var steps int

	for flashed = range SimulateFlashes(grid) {
		steps++
		num += len(flashed)
		if steps == 100 {
			break
		}
	}

	fmt.Fprintln(w, num)

	return nil
}

func SimulateFlashes(grid lib.GridSetter[int]) iter.Seq[lib.Set[lib.Point2]] {
	return func(yield func(lib.Set[lib.Point2]) bool) {
		var extent = grid.Extent()
		var flashed = make(lib.Set[lib.Point2])
		var queue = make([]lib.Point2, 0)

		for {
			var pos lib.Point2

			clear(flashed)
			queue = queue[:0]

			for pos = range extent.AllPoints() {
				// First, the energy level of each octopus increases by 1.
				grid.Set(pos, grid.Get(pos)+1)
				// Then, any octopus with an energy level greater than 9 flashes.
				if grid.Get(pos) > 9 {
					queue = append(queue, pos)
					flashed.Add(pos)
				}
			}

			// This process continues as long as new octopuses keep having their energy level increased beyond 9.
			for len(queue) > 0 {
				var delta lib.Vector2

				pos, queue = queue[0], queue[1:]

				for _, delta[X] = range [3]int{-1, 0, +1} {
					for _, delta[Y] = range [3]int{-1, 0, +1} {
						var adj lib.Point2

						if delta[X] == 0 && delta[Y] == 0 {
							continue
						}

						if adj = pos.Add(delta); !extent.Contains(adj) {
							continue
						}

						if flashed.IsElement(adj) {
							continue
						}

						grid.Set(adj, grid.Get(adj)+1)
						if grid.Get(adj) > 9 {
							queue = append(queue, adj)
							flashed.Add(adj)
						}
					}
				}
			}

			// Finally, any octopus that flashed during this step has its
			// energy level set to 0.
			for pos, _ = range flashed {
				grid.Set(pos, 0)
			}

			if !yield(flashed) {
				return
			}
		}
	}
}
