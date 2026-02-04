package day20_test

import (
	"testing"

	"a12n/advent_of_code/2021/day20"
)

func TestKernelKey(t *testing.T) {
	var kern = day20.Kernel{
		{false, false, false},
		{true, false, false},
		{false, true, false},
	}
	var key = 34

	if kern.ToKey() != key {
		t.Fatalf("ToKey(%v) != %v", kern, key)
	}
}
