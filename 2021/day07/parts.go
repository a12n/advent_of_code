package day07

import (
	"bufio"
	"fmt"
	"io"

	"a12n/advent_of_code/2021/day06"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var positions []int

	if positions, err = day06.ReadNumbers(r); err != nil {
		return err
	}

	fmt.Fprintln(w, MinCost(positions, Absolute))

	return err
}

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var positions []int

	if positions, err = day06.ReadNumbers(r); err != nil {
		return err
	}

	fmt.Fprintln(w, MinCost(positions, func(n int) int { return Triangle(Absolute(n)) }))

	return err
}
