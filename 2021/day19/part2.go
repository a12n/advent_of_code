package day19

import (
	"bufio"
	"io"
	"log"

	"a12n/advent_of_code/2021/lib"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var scanners [][]lib.Point3

	if scanners, err = ReadInput(r); err != nil {
		return err
	}

	log.Println("scanners", scanners)

	return nil
}
