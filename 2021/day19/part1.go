package day19

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"

	"a12n/advent_of_code/2021/lib"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var scanners [][]lib.Point3

	if scanners, err = ReadInput(r); err != nil {
		return err
	}

	for i, beacons := range scanners {
		log.Println(i, "beacons", len(beacons))
		for j := 0; j < len(beacons)-1; j++ {
			for k := j + 1; k < len(beacons); k++ {
				log.Println(i, "distance", beacons[j], beacons[k], beacons[j].DistanceSqr(beacons[k]))
			}
		}
	}

	return nil
}

func ReadInput(r *bufio.Reader) ([][]lib.Point3, error) {
	var beacons []lib.Point3
	var err error
	var scanners [][]lib.Point3

	for {
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
		}

		line = strings.TrimSuffix(line, "\n")

		if beacons != nil {
			if line == "" {
				scanners = append(scanners, beacons)
				beacons = nil
			} else {
				var p lib.Point3

				if _, err = fmt.Sscanf(line, "%d,%d,%d", &p[0], &p[1], &p[2]); err != nil {
					if _, err = fmt.Sscanf(line, "%d,%d", &p[0], &p[1]); err != nil {
						return nil, err
					}
				}

				beacons = append(beacons, p)
			}
		} else {
			var i int

			if _, err = fmt.Sscanf(line, "--- scanner %d ---", &i); err != nil {
				return nil, err
			}

			if i != len(scanners) {
				return nil, fmt.Errorf("scanner %d out of order", i)
			}

			beacons = make([]lib.Point3, 0, 25)
		}
	}

	if len(beacons) != 0 {
		scanners = append(scanners, beacons)
	}

	return scanners, nil
}
