package lib

import (
	"io"
	"slices"
)

type Grid[Elt any] interface {
	Extent() Extent2
	Get(Point2) Elt
	Has(Point2) bool
}

type GridSetter[Elt any] interface {
	Grid[Elt]
	Set(Point2, Elt)
}

type GridPrinter[Elt any] struct {
	Blank    []byte
	NonBlank func(Elt) []byte
	Path     func(Elt) []byte
}

func (p GridPrinter[Elt]) Print(w io.Writer, g Grid[Elt], extent Extent2, path []Point2) error {
	if p.Blank == nil {
		p.Blank = []byte{'.'}
	}
	if p.NonBlank == nil {
		p.NonBlank = func(Elt) []byte { return []byte{'#'} }
	}
	if path != nil && p.Path == nil {
		p.Path = func(Elt) []byte { return []byte{'\x1b', '[', '4', '2', 'm', '#', '\x1b', '[', '0', 'm'} }
	}

	var err error
	var q Point2

	for q[Y] = extent.Begin[Y]; q[Y] < extent.End[Y]; q[Y]++ {
		for q[X] = extent.Begin[X]; q[X] < extent.End[X]; q[X]++ {
			if g.Has(q) {
				if slices.Index(path, q) != -1 {
					_, err = w.Write(p.Path(g.Get(q)))
				} else {
					_, err = w.Write(p.NonBlank(g.Get(q)))
				}
			} else {
				_, err = w.Write(p.Blank)
			}
			if err != nil {
				return err
			}
		}
		if _, err = w.Write([]byte{'\n'}); err != nil {
			return err
		}
	}

	return nil
}
