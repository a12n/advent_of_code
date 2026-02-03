package day03

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var bitSize int
	var err error
	var numbers []int

	if numbers, bitSize, err = ReadNumbers(r); err != nil {
		return err
	}

	log.Println("numbers", numbers, "bitSize", bitSize)

	var co2, oxygen int

	if oxygen, err = FilterNumbers(numbers, bitSize,
		// The most common bit at the position.
		func(num [2]int) int {
			if num[0] > num[1] {
				return 0
			} else {
				return 1
			}
		},
	); err != nil {
		return err
	}
	log.Println("oxygen", oxygen)

	if co2, err = FilterNumbers(numbers, bitSize,
		// The least common bit at the position.
		func(num [2]int) int {
			if num[1] < num[0] {
				return 1
			} else {
				return 0
			}
		},
	); err != nil {
		return err
	}
	log.Println("co2", co2)

	fmt.Fprintln(w, oxygen*co2)

	return nil
}
