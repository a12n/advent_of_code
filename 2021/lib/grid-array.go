package lib

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type ArrayGrid[Elt any] [][]Elt

func (g ArrayGrid[Elt]) Extent() Extent2 {
	if len(g) == 0 || len(g[0]) == 0 {
		return Extent2{}
	}
	return Extent2{End: Point2{Y: len(g), X: len(g[0])}}
}

func (g ArrayGrid[Elt]) Has(p Point2) bool {
	if p[Y] < 0 || p[Y] >= len(g) {
		return false
	}
	if p[X] < 0 || p[X] >= len(g[0]) {
		return false
	}
	return true
}

func (g ArrayGrid[Elt]) Get(p Point2) Elt {
	return g[p[Y]][p[X]]
}

func (g ArrayGrid[Elt]) Set(p Point2, e Elt) {
	g[p[Y]][p[X]] = e
}

func ReadArrayGrid[Elt any](r *bufio.Reader, f func(byte) (Elt, error)) (ArrayGrid[Elt], error) {
	var err error
	var g ArrayGrid[Elt]

	for {
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				return g, nil
			}
			return nil, err
		}

		line = strings.TrimSpace(line)

		if len(g) != 0 && len(g[0]) != len(line) {
			return nil, fmt.Errorf("line %q doesn't match grid row length %d", line, len(g[0]))
		}

		var row = make([]Elt, len(line))

		for i := range line {
			if row[i], err = f(line[i]); err != nil {
				return nil, err
			}
		}

		g = append(g, row)
	}
}

func ReadDigitGrid(r *bufio.Reader) (ArrayGrid[int], error) {
	return ReadArrayGrid(r, func(b byte) (int, error) {
		if b < '0' || b > '9' {
			return 0, strconv.ErrSyntax
		}
		return int(b - '0'), nil
	})
}
