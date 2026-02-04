package lib

// [Begin, End)
type Interval struct {
	Begin, End int
}

func (a Interval) Empty() bool {
	return a.Begin >= a.End
}

func (a Interval) Encloses(b Interval) bool {
	return b.Begin >= a.Begin && b.End <= a.End
}

func (a Interval) Modulo(k int) int {
	k -= a.Begin
	var n = a.Size()
	var r = k % n
	if k < 0 {
		if r == 0 {
			k = 0
		} else {
			k = r + n
		}
	} else {
		k = r
	}
	k += a.Begin
	return k
}

func (a Interval) Overlaps(b Interval) bool {
	return b.Begin < a.End && a.Begin < b.End
}

func (a Interval) Size() int {
	return max(0, a.End-a.Begin)
}

func (a Interval) Intersection(b Interval) Interval {
	if a.Overlaps(b) {
		return Interval{Begin: max(a.Begin, b.Begin), End: min(a.End, b.End)}
	}
	return Interval{}
}

// Merge is like Union, but merges only overlapping/adjacent
// intervals. Returns empty interval otherwise.
func (a Interval) Merge(b Interval) Interval {
	// One of the intervals is empty.
	if a.Empty() {
		return b
	}
	if b.Empty() {
		return a
	}
	// Overlapping intervals.
	if a.Overlaps(b) {
		return Interval{Begin: min(a.Begin, b.Begin), End: max(a.End, b.End)}
	}
	// Adjacent intervals.
	if a.End == b.Begin {
		return Interval{Begin: a.Begin, End: b.End}
	}
	if b.End == a.Begin {
		return Interval{Begin: b.Begin, End: a.End}
	}
	// Disjoint intervals.
	return Interval{}
}

func (a Interval) Union(b Interval) []Interval {
	var u = a.Merge(b)
	// Disjoint intervals, return both in order.
	if u.Empty() {
		if a.End < b.Begin {
			return []Interval{a, b}
		}
		if b.End < a.Begin {
			return []Interval{b, a}
		}
	}
	// Overlapping/adjoint, return the union.
	return []Interval{u}
}

func (a Interval) Difference(b Interval) []Interval {
	var i = a.Intersection(b)

	// No intersection, result is the entire interval.
	if i.Empty() {
		return []Interval{a}
	}

	var l, r *Interval

	if i.Begin > a.Begin {
		// There's a part of the interval to the left of the intersection:
		// a = [------------…
		// i =          [---…
		// l = [--------)
		l = &Interval{Begin: a.Begin, End: i.Begin}
	}
	if i.End < a.End {
		// There's a part to the right of the intersection:
		// a = …------------)
		// i = …---)
		// r =     [--------)
		r = &Interval{Begin: i.End, End: a.End}
	}

	if l == nil && r == nil {
		// No left or right interval parts, the entire interval
		// subtracted.
		return nil
	} else if r == nil {
		// Only left part, the interval is trimmed from the right.
		return []Interval{*l}
	} else if l == nil {
		// Only right part, the interval is trimmed from the left.
		return []Interval{*r}
	} else {
		// Both parts present, a section in the middle of the interval
		// is subtracted.
		return []Interval{*l, *r}
	}
}
