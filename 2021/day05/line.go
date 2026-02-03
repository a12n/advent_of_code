package day05

import (
	"bufio"
	"fmt"
	"iter"
)

type Point struct {
	X, Y int
}

type Line struct {
	Begin, End Point
}

func ReadLine(r *bufio.Reader) (Line, error) {
	var err error
	var l Line

	if _, err = fmt.Fscanf(r,
		"%d,%d -> %d,%d\n",
		&l.Begin.X, &l.Begin.Y, &l.End.X, &l.End.Y,
	); err != nil {
		return Line{}, err
	}

	return l, nil
}

func (l *Line) IsHoriz() bool {
	return l.Begin.Y == l.End.Y
}

func (l *Line) IsVert() bool {
	return l.Begin.X == l.End.X
}

func (l *Line) AllPoints() iter.Seq[Point] {
	var dX, dY int

	if l.Begin.X < l.End.X {
		dX = 1
	} else if l.Begin.X > l.End.X {
		dX = -1
	}

	if l.Begin.Y < l.End.Y {
		dY = 1
	} else if l.Begin.Y > l.End.Y {
		dY = -1
	}

	return func(yield func(Point) bool) {
		var x, y = l.Begin.X, l.Begin.Y
		for {
			if !yield(Point{x, y}) || (x == l.End.X && y == l.End.Y) {
				return
			}
			x += dX
			y += dY
		}
	}
}
