package day15

import (
	"bufio"
	"fmt"
	"io"

	"a12n/advent_of_code/2021/lib"
)

const (
	X = iota
	Y
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid TileGrid

	if grid.ArrayGrid, err = lib.ReadDigitGrid(r); err != nil {
		return err
	}

	var extent = grid.Extent()

	fmt.Fprintln(w, LowestTotalRisk(grid, extent, extent.Begin, extent.MaxPoint()))

	return err

}

type TileGrid struct {
	lib.ArrayGrid[int]
}

func (g TileGrid) Extent() lib.Extent2 {
	var extent = g.ArrayGrid.Extent()
	return lib.Extent2{
		End: lib.Point2{
			X: extent.Size(X) * 5,
			Y: extent.Size(Y) * 5,
		},
	}
}

func (g TileGrid) Has(p lib.Point2) bool {
	return g.Extent().Contains(p)
}

func (g TileGrid) Get(p lib.Point2) int {
	var extent = g.ArrayGrid.Extent()

	// "each time the tile repeats to the right or downward, all of its
	// risk levels are 1 higher than the tile immediately up or left
	// of it".
	var riskIncr = lib.Point2{
		X: p[X] / extent.Size(X),
		Y: p[Y] / extent.Size(Y),
	}.SubP(lib.Point2{}).TaxicabNorm()

	var modPos = lib.Point2{
		X: p[X] % extent.Size(X),
		Y: p[Y] % extent.Size(Y),
	}

	var origRisk = g.ArrayGrid.Get(modPos)
	var risk = origRisk + riskIncr

	if risk > 9 {
		risk = risk%10 + 1
	}

	return risk
}
