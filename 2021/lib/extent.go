package lib

import "iter"

type Extent2 struct {
	Begin, End Point2
}

func (a Extent2) AllPoints() iter.Seq[Point2] {
	return func(yield func(Point2) bool) {
		var p Point2
		for p[X] = a.Begin[X]; p[X] < a.End[X]; p[X]++ {
			for p[Y] = a.Begin[Y]; p[Y] < a.End[Y]; p[Y]++ {
				if !yield(p) {
					return
				}
			}
		}
	}
}

func (a Extent2) Area() int {
	return a.Size(X) * a.Size(Y)
}

func (a Extent2) Contains(p Point2) bool {
	return p[X] >= a.Begin[X] && p[X] < a.End[X] &&
		p[Y] >= a.Begin[Y] && p[Y] < a.End[Y]
}

func (a Extent2) Expand(d int) Extent2 {
	// Negative d?
	return Extent2{Begin: a.Begin.Add(Vector2{-d, -d}), End: a.End.Add(Vector2{d, d})}
}

func (a Extent2) MaxPoint() Point2 {
	return a.End.SubV(Vector2{1, 1})
}

func (a Extent2) Modulo(dim Axis, p Point2) Point2 {
	p[dim] = Interval{a.Begin[dim], a.End[dim]}.Modulo(p[dim])
	return p
}

func (a Extent2) Size(dim Axis) int {
	return max(0, a.End[dim]-a.Begin[dim])
}

// [Begin, End)
type Extent3 struct {
	Begin, End Point3
}

func (a Extent3) AllPoints() iter.Seq[Point3] {
	return func(yield func(Point3) bool) {
		var p Point3
		for p[X] = a.Begin[X]; p[X] < a.End[X]; p[X]++ {
			for p[Y] = a.Begin[Y]; p[Y] < a.End[Y]; p[Y]++ {
				for p[Z] = a.Begin[Z]; p[Z] < a.End[Z]; p[Z]++ {
					if !yield(p) {
						return
					}
				}
			}
		}
	}
}

func (a Extent3) Contain(p Point3) Extent3 {
	return Extent3{
		Begin: Point3{
			min(a.Begin[X], p[X]),
			min(a.Begin[Y], p[Y]),
			min(a.Begin[Z], p[Z]),
		},
		End: Point3{
			max(a.End[X], p[X]+1),
			max(a.End[Y], p[Y]+1),
			max(a.End[Z], p[Z]+1),
		},
	}
}

func (a Extent3) Contains(p Point3) bool {
	return p[X] >= a.Begin[X] && p[X] < a.End[X] &&
		p[Y] >= a.Begin[Y] && p[Y] < a.End[Y] &&
		p[Z] >= a.Begin[Z] && p[Z] < a.End[Z]
}

func (a Extent3) Empty() bool {
	return a.Begin[X] >= a.End[X] ||
		a.Begin[Y] >= a.End[Y] ||
		a.Begin[Z] >= a.End[Z]
}

func (a Extent3) Enclose(b Extent3) Extent3 {
	return Extent3{
		Begin: Point3{
			min(a.Begin[X], b.Begin[X]),
			min(a.Begin[Y], b.Begin[Y]),
			min(a.Begin[Z], b.Begin[Z]),
		},
		End: Point3{
			max(a.End[X], b.End[X]),
			max(a.End[Y], b.End[Y]),
			max(a.End[Z], b.End[Z]),
		},
	}
}

func (a Extent3) Encloses(b Extent3) bool {
	return a.Contains(b.Begin) && a.Contains(b.MaxPoint())
}

func (a Extent3) MaxPoint() Point3 {
	return a.End.SubV(Vector3{1, 1, 1})
}

func (a Extent3) Overlaps(b Extent3) bool {
	return a.ProjOn(X).Overlaps(b.ProjOn(X)) &&
		a.ProjOn(Y).Overlaps(b.ProjOn(Y)) &&
		a.ProjOn(Z).Overlaps(b.ProjOn(Z))
}

func (a Extent3) ProjAlong(dim Axis) Extent2 {
	return Extent2{
		Begin: Point2{
			a.Begin[(dim+1)%3],
			a.Begin[(dim+2)%3],
		},
		End: Point2{
			a.End[(dim+1)%3],
			a.End[(dim+2)%3],
		},
	}
}

func (a Extent3) ProjOn(dim Axis) Interval {
	return Interval{a.Begin[dim], a.End[dim]}
}

func (a Extent3) Size(dim Axis) int {
	return max(0, a.End[dim]-a.Begin[dim])
}

func (a Extent3) Volume() int {
	return a.Size(X) * a.Size(Y) * a.Size(Z)
}

func (a Extent3) Intersection(b Extent3) Extent3 {
	return Extent3{
		Begin: Point3{
			X: max(a.Begin[X], b.Begin[X]),
			Y: max(a.Begin[Y], b.Begin[Y]),
			Z: max(a.Begin[Z], b.Begin[Z]),
		},
		End: Point3{
			X: min(a.End[X], b.End[X]),
			Y: min(a.End[Y], b.End[Y]),
			Z: min(a.End[Z], b.End[Z]),
		},
	}
}

// Merge is like union, but for overlapping/adjacent regions.
func (a Extent3) Merge(b Extent3) Extent3 {
	if b.Empty() {
		return a
	}
	if a.Empty() {
		return b
	}

	if a.Encloses(b) {
		return a
	}
	if b.Encloses(a) {
		return b
	}

	// If YZ (ZX, XY) of the first region is the same as YZ (ZX, XY)
	// of the second region, and X (Y, Z) of the first overlaps X (Y,
	// Z) of the second, merge regions into one.
	if a.Overlaps(b) &&
		(a.ProjAlong(X) == b.ProjAlong(X) ||
			a.ProjAlong(Y) == b.ProjAlong(Y) ||
			a.ProjAlong(Z) == b.ProjAlong(Z)) {
		return a.Enclose(b)
	}

	// Can't merge into one axis-aligned region, return empty.
	return Extent3{}
}

func (a Extent3) Union(b Extent3) []Extent3 {
	if a.Encloses(b) {
		return []Extent3{a}
	}
	if b.Encloses(a) {
		return []Extent3{b}
	}

	var i = a.Intersection(b)

	if i.Empty() {
		return []Extent3{a, b}
	}

	return append([]Extent3{a}, b.Difference(i)...)
}

func (a Extent3) Difference(b Extent3) []Extent3 {
	var intxn = a.Intersection(b)

	if intxn.Empty() {
		// There's no intersection, the region is left unmodified by
		// subtraction.
		return []Extent3{a}
	}

	// There may left up to 3**(dim)-1 regions after the subtraction
	// (the intersection in the middle isn't included):
	//
	// +-------.---------.--------+
	// | 1     . 2       . 3      |
	// ........+---------+.........
	// | 4     |#########| 5      |
	// ........+---------+.........
	// | 6     . 7       . 8      |
	// +-------.---------.--------+
	//
	// Check all of these regions for Empty(), add non-empty to the result.
	var coords = [3][4]int{
		X: [4]int{a.Begin[X], intxn.Begin[X], intxn.End[X], a.End[X]},
		Y: [4]int{a.Begin[Y], intxn.Begin[Y], intxn.End[Y], a.End[Y]},
		Z: [4]int{a.Begin[Z], intxn.Begin[Z], intxn.End[Z], a.End[Z]},
	}
	var i, j, k int
	var region Extent3
	var result []Extent3

	for i = 0; i < len(coords[X])-1; i++ {
		for j = 0; j < len(coords[Y])-1; j++ {
			for k = 0; k < len(coords[Z])-1; k++ {
				region = Extent3{
					Begin: Point3{coords[X][i], coords[Y][j], coords[Z][k]},
					End:   Point3{coords[X][i+1], coords[Y][j+1], coords[Z][k+1]},
				}

				if region == intxn || region.Empty() {
					continue
				}

				result = append(result, region)
			}
		}
	}

	// TODO: Merge

	return result
}
