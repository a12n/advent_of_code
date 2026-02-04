package day22

import "a12n/advent_of_code/2021/lib"

type RegionSet []lib.Extent3

func (s *RegionSet) Add(r lib.Extent3) {
	var rs = RegionSet{r}
	var t lib.Extent3
	for _, t = range *s {
		rs.Sub(t)
	}
	*s = append(*s, rs...)
}

func (s *RegionSet) Sub(r lib.Extent3) {
	var t lib.Extent3
	var tmp RegionSet
	for _, t = range *s {
		tmp = append(tmp, t.Difference(r)...)
	}
	*s = tmp
}

func (s RegionSet) Volume() int {
	var t lib.Extent3
	var sum int

	for _, t = range s {
		sum += t.Volume()
	}

	return sum
}
