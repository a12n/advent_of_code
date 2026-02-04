package day20

import (
	"bufio"
	"fmt"
	"io"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var alg *Algorithm
	var err error
	var image Image

	if alg, image, err = ReadInput(r); err != nil {
		return err
	}

	var outside bool
	image, outside = image.Enhance(outside, alg)
	image, outside = image.Enhance(outside, alg)
	fmt.Fprintln(w, len(image.MapGrid))

	return err
}

func ReadInput(r *bufio.Reader) (*Algorithm, Image, error) {
	var alg *Algorithm
	var err error
	var image Image

	if alg, err = ReadAlgorithm(r); err != nil {
		return nil, Image{}, err
	}

	if _, err = r.ReadString('\n'); err != nil {
		return nil, Image{}, err
	}

	if image, err = ReadImage(r); err != nil {
		return nil, Image{}, err
	}

	return alg, image, nil
}
