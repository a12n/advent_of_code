package day23_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"a12n/advent_of_code/2021/day23"
)

func TestPath(t *testing.T) {
	var assert = assert.New(t)

	var dist int
	var path []day23.Spot

	path, dist = day23.H3.Path(day23.H3)
	assert.Empty(path)
	assert.Zero(dist)

	path, dist = day23.A1.Path(day23.C1)
	assert.Equal([]day23.Spot{day23.A0, day23.H2, day23.H3, day23.C0, day23.C1}, path)
	assert.Equal(8, dist)

	path, dist = day23.H0.Path(day23.H6)
	assert.Equal([]day23.Spot{day23.H1, day23.H2, day23.H3, day23.H4, day23.H5, day23.H6}, path)
	assert.Equal(10, dist)

	path, dist = day23.H6.Path(day23.D1)
	assert.Equal([]day23.Spot{day23.H5, day23.D0, day23.D1}, path)
	assert.Equal(4, dist)

	path, dist = day23.C1.Path(day23.B0)
	assert.Equal([]day23.Spot{day23.C0, day23.H3, day23.B0}, path)
	assert.Equal(5, dist)

	path, dist = day23.A2.Path(day23.H6)
	assert.Equal([]day23.Spot{day23.A1, day23.A0, day23.H2, day23.H3, day23.H4, day23.H5, day23.H6}, path)
	assert.Equal(11, dist)

	path, dist = day23.C3.Path(day23.B3)
	assert.Equal([]day23.Spot{day23.C2, day23.C1, day23.C0, day23.H3, day23.B0, day23.B1, day23.B2, day23.B3}, path)
	assert.Equal(10, dist)
}

func TestDistance(t *testing.T) {
	var dist int
	var ok bool

	// A1D1-A0H2-B0C1-B1D0
	ok, dist = day23.AmphipodSpotMap{
		{day23.A1, day23.D1},
		{day23.A0, day23.H2},
		{day23.B0, day23.C1},
		{day23.B1, day23.D0},
	}.Distance(day23.C, day23.B0, day23.C0)
	assert.True(t, ok)
	assert.Equal(t, 4, dist)
}
