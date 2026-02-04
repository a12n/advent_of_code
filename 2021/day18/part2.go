package day18

import (
	"bufio"
	"fmt"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var trees []*Node

	if trees, err = ReadTrees(r); err != nil {
		return err
	}

	var i, j int
	var largest int

	for i = range trees {
		for j = range trees {
			if i == j {
				continue
			}

			var sum = Add(trees[i].Copy(), trees[j].Copy())
			sum.Reduce()
			largest = max(largest, sum.Magnitude())
		}
	}

	fmt.Fprintln(w, largest)

	return nil
}
