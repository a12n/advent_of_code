package day06

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strconv"
	"strings"
)

const (
	MaxAge = 8
)

func Part(r *bufio.Reader, w io.Writer, days int) error {
	var err error
	var fish []int
	if fish, err = ReadNumbers(r); err != nil {
		return err
	}
	fmt.Fprintln(w, Simulate(fish, days))
	return nil
}

func ReadNumbers(r *bufio.Reader) ([]int, error) {
	var err error
	var numbers []int
	var str string

	if str, err = r.ReadString('\n'); err != nil {
		return nil, err
	}

	for _, str = range strings.Split(strings.TrimSpace(str), ",") {
		var n uint64

		if n, err = strconv.ParseUint(str, 10, 32); err != nil {
			return nil, err
		}

		numbers = append(numbers, int(n))
	}

	return numbers, nil
}

func Simulate(fish []int, days int) int {
	var age int
	var ages [2][MaxAge + 1]int
	var n int

	// Build age frequency table.
	for _, age = range fish {
		ages[days%2][age]++
	}

	for ; days > 0; days-- {
		log.Println("days", days, "ages", ages[days%2])

		// Fish of age 0 create fish of age 8 and reset to age 6 at T+1.
		ages[(days-1)%2][6] = ages[days%2][0]
		ages[(days-1)%2][8] = ages[days%2][0]
		// Clear age 0 in table at T+2.
		ages[days%2][0] = 0

		for age = 1; age < (MaxAge + 1); age++ {
			// Fish of age N create fish of age (N - 1) at T+1.
			ages[(days-1)%2][age-1] += ages[days%2][age]
			// Clear age N in table at T+2.
			ages[days%2][age] = 0
		}
	}

	// Reduce frequency table to number of fish.
	for age = 0; age < (MaxAge + 1); age++ {
		n += ages[0][age]
	}

	return n
}
