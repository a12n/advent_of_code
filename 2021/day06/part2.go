package day06

import (
	"bufio"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	return Part(r, w, 256)
}
