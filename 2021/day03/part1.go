package day03

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var bitSize, i int
	var err error
	var gamma, epsilon int
	var numbers []int

	if numbers, bitSize, err = ReadNumbers(r); err != nil {
		return err
	}

	log.Println("numbers", numbers, "bitSize", bitSize)

	for i = bitSize - 1; i >= 0; i-- {
		var numBits = CountBits(numbers, bitSize, i)

		if numBits[1] > numBits[0] {
			gamma = gamma | (1 << i)
		} else {
			epsilon = epsilon | (1 << i)
		}

		log.Println("i", i, "numBits", numBits, "gamma", gamma, "epsilon", epsilon)
	}

	fmt.Fprintln(w, gamma*epsilon)

	return nil
}
