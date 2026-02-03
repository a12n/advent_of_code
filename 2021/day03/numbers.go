package day03

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func ReadNumbers(r *bufio.Reader) ([]int, int, error) {
	var bitSize int
	var err error
	var numbers []int

	for {
		var line string
		var n uint64

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return nil, 0, err
		}
		line = strings.TrimSpace(line)

		if n, err = strconv.ParseUint(line, 2, 16); err != nil {
			return nil, 0, err
		}

		if bitSize == 0 {
			bitSize = len(line)
		}

		numbers = append(numbers, int(n))
	}

	return numbers, bitSize, nil
}

// Reorders numbers in-place.
func FilterNumbers(numbers []int, bitSize int, criteria func([2]int) int) (int, error) {
	var i, n int

	for i = bitSize - 1; i >= 0; i-- {
		var bit = criteria(CountBits(numbers, bitSize, i))

		// Select `n` numbers with `bit` value at the position `i`.
		if n = PartitionNumbers(numbers, func(k int) bool {
			return ((k >> i) & 1) == bit
		}); n == 1 {
			return numbers[0], nil
		}

		// Next time consider only these selected numbers.
		numbers = numbers[:n]
	}

	return 0, fmt.Errorf("%d numbers selected by bit criteria", n)
}

// Reorders numbers in-place.
func PartitionNumbers(numbers []int, pred func(int) bool) int {
	var i, n int

	for i = range numbers {
		if pred(numbers[i]) {
			numbers[n], numbers[i] = numbers[i], numbers[n]
			n++
		}
	}

	return n
}

func CountBits(numbers []int, bitSize, i int) [2]int {
	var n int
	var numBits [2]int
	for _, n = range numbers {
		numBits[(n>>i)&1]++
	}
	return numBits
}
