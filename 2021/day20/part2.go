package day20

import (
	"bufio"
	"fmt"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var alg *Algorithm
	var err error
	var image Image

	if alg, image, err = ReadInput(r); err != nil {
		return err
	}

	var outside bool

	for i := 0; i < 50; i++ {
		image, outside = image.Enhance(outside, alg)
	}

	fmt.Fprintln(w, len(image.MapGrid))

	return nil
}
