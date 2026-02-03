package day07

import (
	"log"
	"math"
	"slices"
)

type CostFunc func(int) int

func MinCost(positions []int, cost CostFunc) int {
	var best = math.MaxInt
	var from, to = slices.Min(positions), slices.Max(positions)

	for ; from <= to; from++ {
		var fuel, p int

		for _, p = range positions {
			fuel += cost(p - from)
			if fuel > best {
				break
			}
		}

		if fuel < best {
			best = fuel
			log.Println("best", best)
		}
	}

	return best
}

func Absolute(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func Triangle(n int) int {
	return n * (n + 1) / 2
}
