package lib

import (
	"bufio"
	"io"
	"strings"
)

type MapGrid[Elt any] map[Point2]Elt

func (g MapGrid[Elt]) Extent() (e Extent2) {
	for p, _ := range g {
		e.Begin[X] = min(e.Begin[X], p[X])
		e.Begin[Y] = min(e.Begin[Y], p[Y])
		e.End[X] = max(e.End[X], p[X]+1)
		e.End[Y] = max(e.End[Y], p[Y]+1)
	}
	return
}

func (g MapGrid[Elt]) Has(p Point2) (ok bool) {
	_, ok = g[p]
	return
}

func (g MapGrid[Elt]) Get(p Point2) Elt {
	return g[p]
}

func (g MapGrid[Elt]) Set(p Point2, e Elt) {
	g[p] = e
}

func ReadMapGrid[Elt any](r *bufio.Reader, orig Point2, parse func(byte) (*Elt, error)) (MapGrid[Elt], error) {
	var g = make(MapGrid[Elt])
	var u Vector2

	for ; ; u[Y]++ {
		var elt *Elt
		var err error
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				return g, nil
			}
			return nil, err
		}

		for u[X] = range strings.TrimSuffix(line, "\n") {
			if elt, err = parse(line[u[X]]); err != nil {
				return nil, err
			}
			if elt != nil {
				g[orig.Add(u)] = *elt
			}
		}
	}
}
